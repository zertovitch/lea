
with GWindows.Types,
     GWindows.Base,
     GWindows.Application,
     GWindows.Errors;

with Ada.Command_Line,
     Ada.Unchecked_Conversion;

with Interfaces.C;

with Memory_Streams;
with Ada.Streams;
with System.Storage_Elements;

package body LEA_GWin.Single_Instance is

  IPC_BUFFER_SIZE : constant := 1024;

  WM_COPYDATA : constant := 74;
  WM_USER     : constant := 16#400#;

  IDI_APPLICATION : constant := 32512;

  type COPY_DATA_STRUCT is record
    dwData : GWindows.Types.DWORD_PTR;   -- User defined value
    cbData : GWindows.Types.DWORD;       -- Size of Data
    lpData : System.Address;             -- Data pointer
  end record;

  type COPY_DATA_STRUCT_PTR is access all COPY_DATA_STRUCT;

  COPY_DATA_CMD_LINE : constant := 1;

  type BOOL is new Interfaces.C.int;
  type UINT is new Interfaces.C.unsigned;

  procedure Register_Class_Name;
  function Is_First_Instance return Boolean;
  function Find_Window (Class_Name  : GWindows.GString) return GWindows.Types.Handle;
  function Is_Zoomed (hWnd : GWindows.Types.Handle) return Boolean;
  function Is_Iconic (hWnd : GWindows.Types.Handle) return Boolean;
  procedure Set_Foreground_Window (hWnd : GWindows.Types.Handle);
  procedure Show_Window (hWnd : GWindows.Types.Handle; Cmd : Integer);

  function LEA_WndProc (hwnd    : GWindows.Types.Handle;
                        message : Interfaces.C.unsigned;
                        wParam  : GWindows.Types.Wparam;
                        lParam  : GWindows.Types.Lparam)
                       return GWindows.Types.Lresult;
  pragma Convention (Stdcall, LEA_WndProc);

  Top_Wnd : LEA_GWin.MDI_Main.MDI_Main_Access;

  --  *************************************************************************
  procedure Manage_Single_Instance (Top_Window     : LEA_GWin.MDI_Main.MDI_Main_Access;
                                    Exit_Requested : out Boolean) is
    First_Instance : constant Boolean := Is_First_Instance;
  begin
    Exit_Requested := False;
    Top_Wnd        := Top_Window;

    Register_Class_Name;

    if not First_Instance then
      declare
        use type GWindows.Types.Handle;

        Window_Handle : GWindows.Types.Handle;
      begin
        declare
          Nb_Retry : Natural := 15;
        begin
          loop
            Window_Handle := Find_Window (LEA_Class_Name);
            exit when Window_Handle /= GWindows.Types.Null_Handle;
            Nb_Retry := Nb_Retry - 1;
            exit when Nb_Retry = 0;
            delay 0.1;
          end loop;
        end;

        if Window_Handle /= GWindows.Types.Null_Handle then
          --  Restore the window, bring it to front, etc
          declare
            SW_MAXIMIZE : constant := 3;
            SW_RESTORE  : constant := 9;
            sw : Integer := 0;
          begin
            if Is_Zoomed (Window_Handle) then
              sw := SW_MAXIMIZE;
            elsif Is_Iconic (Window_Handle) then
              sw := SW_RESTORE;
            end if;
            if sw /= 0 then
              Show_Window (Window_Handle, sw);
            end if;
          end;
          Set_Foreground_Window (Window_Handle);

          --  Transfert the command line arguments to the first instance
          if Ada.Command_Line.Argument_Count > 0 then
            declare
              use type GWindows.Types.DWORD;
              use type Ada.Streams.Stream_Element_Offset;

              Copy_Data : COPY_DATA_STRUCT;

              procedure SendMessage (hwnd   : GWindows.Types.Handle;
                                     uMsg   : Interfaces.C.int;
                                     wParam : GWindows.Types.Wparam;
                                     lParam : access COPY_DATA_STRUCT);
              pragma Import (StdCall, SendMessage, "SendMessage" & GWindows.Character_Mode_Identifier);
              Stream : aliased Memory_Streams.Memory_Stream;
              Buffer : System.Storage_Elements.Storage_Array (1 .. IPC_BUFFER_SIZE) := (others => 0);

            begin
              Memory_Streams.Set_Address (Stream, Buffer (1)'Address, Buffer'Length);

              Natural'Output (Stream'Access, Ada.Command_Line.Argument_Count);
              for i in 1 .. Ada.Command_Line.Argument_Count loop
                String'Output (Stream'Access, Ada.Command_Line.Argument (i));
              end loop;

              Copy_Data.dwData := COPY_DATA_CMD_LINE;
              Copy_Data.cbData := Buffer'Length;
              Copy_Data.lpData := Buffer (1)'Address;

              SendMessage (Window_Handle,
                           WM_COPYDATA,
                           0,
                           Copy_Data'Unrestricted_Access);
            end;
          end if;

          Exit_Requested := True;
        end if;
      end;
    end if;
  end Manage_Single_Instance;

  --  *************************************************************************
  procedure Register_Class_Name is
      function LoadIcon (hInstance  : GWindows.Types.Handle := GWindows.Types.Null_Handle;
                         lpIconName : Integer := IDI_APPLICATION)
                        return GWindows.Types.Handle;
      pragma Import (StdCall, LoadIcon, "LoadIcon" & GWindows.Character_Mode_Identifier);

      Window_Class_Name_C : constant GWindows.GString_C := GWindows.GStrings.To_GString_C (LEA_Class_Name);
      Window_Class        : GWindows.Base.WNDCLASS;
  begin
      Window_Class.hInstance     := GWindows.Application.hInstance;
      Window_Class.hIcon         := LoadIcon;
      Window_Class.lpszClassName := Window_Class_Name_C (Window_Class_Name_C'First)'Unrestricted_Access;
      Window_Class.lpfnWndProc   := LEA_WndProc'Address;
      GWindows.Base.Register_Class (Window_Class);
  end Register_Class_Name;

  --  *************************************************************************
  --  Returns True if this LEA instance is the first to be running
  function Is_First_Instance return Boolean is

    type BOOL is new Interfaces.C.int;

    function CreateMutex (MutexAttributes : System.Address := System.Null_Address;
                          InitialOwner    : BOOL           := 0; -- False
                          Name            : GWindows.GString_C)
                         return GWindows.Types.Handle;
    pragma Import (StdCall, CreateMutex, "CreateMutex" & GWindows.Character_Mode_Identifier);

    Mutex_Name   : constant GWindows.GString_C := GWindows.GStrings.To_GString_C ("LEA_Editor_Instance");
    Mutex_Handle : GWindows.Types.Handle;
    Error        : Integer;
    pragma Unreferenced (Mutex_Handle);

    ERROR_ALREADY_EXISTS : constant := 183;

  begin
    --  If the Mutex creation fails, then LEA is already running.
    Mutex_Handle := CreateMutex (Name => Mutex_Name);
    Error := GWindows.Errors.Get_Last_Error;
    return not (Error = ERROR_ALREADY_EXISTS);
  end Is_First_Instance;

  --  *************************************************************************
  --  Returns the handle of the first LEA instance Window or NULL
  function Find_Window (Class_Name  : GWindows.GString)
                        --  Window_Name : GWindows.GString)
                       return GWindows.Types.Handle is

    function FindWindow (ClassName  : GWindows.GString_C;
                         --  WindowName : GWindows.GString_C)
                         WindowName : System.Address := System.Null_Address)
                        return GWindows.Types.Handle;
    pragma Import (StdCall, FindWindow,
                   "FindWindow" & GWindows.Character_Mode_Identifier);

    ClassName  : constant GWindows.GString_C := GWindows.GStrings.To_GString_C (Class_Name);
    --  WindowName : constant GWindows.GString_C := GWindows.GStrings.To_GString_C (Window_Name);
  begin
    return FindWindow (ClassName); --  , WindowName);
  end Find_Window;

  --  ************************************************************************
  function Is_Zoomed (hWnd : GWindows.Types.Handle) return Boolean is
    function IsZoomed (hWnd : GWindows.Types.Handle) return BOOL;
    pragma Import (StdCall, IsZoomed, "IsZoomed");
  begin
    return IsZoomed (hWnd) /= 0;
  end Is_Zoomed;

  --  ************************************************************************
  function Is_Iconic (hWnd : GWindows.Types.Handle) return Boolean is
    function IsIconic (hWnd : GWindows.Types.Handle) return BOOL;
    pragma Import (StdCall, IsIconic, "IsIconic");
  begin
    return IsIconic (hWnd) /= 0;
  end Is_Iconic;

  --  ************************************************************************
  procedure Set_Foreground_Window (hWnd : GWindows.Types.Handle) is
    procedure SetForegroundWindow (hWnd : GWindows.Types.Handle);
    pragma Import (StdCall, SetForegroundWindow, "SetForegroundWindow");
  begin
    SetForegroundWindow (hWnd);
  end Set_Foreground_Window;

  --  ************************************************************************
  procedure Show_Window (hWnd : GWindows.Types.Handle;
                         Cmd  : Integer)
  is
    procedure ShowWindow (hwnd     : GWindows.Types.Handle;
                          nCmdShow : Interfaces.C.long);
    pragma Import (StdCall, ShowWindow, "ShowWindow");
  begin
    ShowWindow (hWnd, Interfaces.C.long (Cmd));
  end Show_Window;

  --  ************************************************************************
  function LEA_WndProc (hwnd    : GWindows.Types.Handle;
                        message : Interfaces.C.unsigned;
                        wParam  : GWindows.Types.Wparam;
                        lParam  : GWindows.Types.Lparam)
                       return GWindows.Types.Lresult is
  begin
    case message is
      when WM_COPYDATA =>
        declare
          use type Ada.Streams.Stream_Element_Offset;

          function To_COPY_DATA is new Ada.Unchecked_Conversion (GWindows.Types.Lparam, COPY_DATA_STRUCT_PTR);

          Copy_Data : constant COPY_DATA_STRUCT_PTR := To_COPY_DATA (lParam);
        begin
          case Copy_Data.dwData is
            when COPY_DATA_CMD_LINE =>
              declare
                Stream : aliased Memory_Streams.Memory_Stream;
                Nb_Params : Integer;
                start_line : Integer := -1;
              begin
                Memory_Streams.Set_Address (Stream, Copy_Data.lpData, IPC_BUFFER_SIZE);
                Nb_Params := Natural'Input (Stream'Access);

                for i in 1 .. Nb_Params loop
                  declare
                    a : constant String := String'Input (Stream'Access);
                  begin
                    if a (a'First) = '+' then  --  Emacs +linenum
                      start_line := 0;
                      for j in a'First + 1 .. a'Last loop
                        if a (j) in '0' .. '9' then
                          start_line := start_line * 10 + (Character'Pos (a (j)) - Character'Pos ('0'));
                        else
                          start_line := -1;  --  Invalid number
                          exit;
                        end if;
                      end loop;
                    else
                      Top_Wnd.Open_Child_Window_And_Load
                                (LEA_Common.To_UTF_16 (a),
                                 start_line - 1);  --  NB: Scintilla lines are 0-based
                      start_line := -1;
                    end if;
                  end;
                end loop;
              end;
              return 1;

            when others =>
              return 0;
          end case;
        end;

      when others =>
        return GWindows.Base.WndProc (hwnd    => hwnd,
                                      message => message,
                                      wParam  => wParam,
                                      lParam  => lParam);
    end case;
  end LEA_WndProc;

end LEA_GWin.Single_Instance;