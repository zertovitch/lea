with GWindows.Application,
     GWindows.Common_Controls,
     GWindows.Constants,
     GWindows.Drawing_Objects,
     GWindows.Types;

with Ada.Environment_Variables,
     Ada.Integer_Text_IO,
     Ada.Text_IO;

with GNAT.OS_Lib;
with Interfaces.C;
with System;

pragma Elaborate_All(GWindows.Drawing_Objects);  --  For GUI_Font initialisation

package body GWin_Util is

  function To_Lower (Value : GString) return GString is
    s : GString := Value;
  begin
    GWindows.GStrings.To_Lower (s);
    return s;
  end To_Lower;

  package Common_Fonts is
    GUI_Font : GWindows.Drawing_Objects.Font_Type;
    URL_Font : GWindows.Drawing_Objects.Font_Type;
    -- ^ These fonts are created once, at startup
    --   it avoid GUI resource leak under Windows 95/98/ME
    procedure Create_Common_Fonts;
    -- in initialisation part if this pkg becomes standalone
  end Common_Fonts;

  package body Common_Fonts is

    procedure Create_Common_Fonts is
      use Interfaces.C;

      type Face_Name_Type is array(1..32) of GWindows.GChar_C;

      type LOGFONT is record
        lfHeight: Interfaces.C.long;
        lfWidth: Interfaces.C.long;
        lfEscapement: Interfaces.C.long;
        lfOrientation: Interfaces.C.long;
        lfWeight: Interfaces.C.long;
        lfItalic: Interfaces.C.char;
        lfUnderline: Interfaces.C.char;
        lfStrikeOut: Interfaces.C.char;
        lfCharSet: Interfaces.C.char;
        lfOutPrecision: Interfaces.C.char;
        lfClipPrecision: Interfaces.C.char;
        lfQuality: Interfaces.C.char;
        lfPitchAndFamily: Interfaces.C.char;
        lfFaceName: Face_Name_Type;
      end record;

      Log_of_current_font: aliased LOGFONT;

      subtype PVOID   is System.Address;                      --  winnt.h
      subtype LPVOID  is PVOID;                               --  windef.h

      function GetObject
        (hgdiobj  : GWindows.Types.Handle  := GWindows.Drawing_Objects.Handle(GUI_Font);
         cbBufferl: Interfaces.C.int       := LOGFONT'Size / 8;
         lpvObject: LPVOID                 := Log_of_current_font'Address)
        return Interfaces.C.int;
      pragma Import (StdCall, GetObject,
                       "GetObject" & Character_Mode_Identifier);

      function CreateFontIndirect
        (lpvObject: LPVOID                 := Log_of_current_font'Address)
        return GWindows.Types.Handle;
      pragma Import (StdCall, CreateFontIndirect,
                       "CreateFontIndirect" & Character_Mode_Identifier);

    begin
      GWindows.Drawing_Objects.Create_Stock_Font(
        GUI_Font,
        GWindows.Drawing_Objects.Default_GUI
      );
      if GetObject = 0 then
        GWindows.Drawing_Objects.Create_Font(URL_Font,
          "MS Sans Serif",
          14, Underline => True);
            -- !! ^ Not so nice (non-unsharpened font, size ~..., color ?)
      else
        Log_of_current_font.lfUnderline:= Interfaces.C.char'Val(1);
        GWindows.Drawing_Objects.Handle(URL_Font, CreateFontIndirect);
      end if;
    end Create_Common_Fonts;

  end Common_Fonts;

  procedure Use_GUI_Font(Window: in out GWindows.Base.Base_Window_Type'Class)
  is
  begin
    --  Use Standard Windows GUI font instead of system font
    GWindows.Base.Set_Font (Window, Common_Fonts.GUI_Font);
  end Use_GUI_Font;

  function To_URL_Encoding( s: String ) return String is
    p: Integer;
    r: String(1..s'Length*3);
    j: Integer;
    hex: String(1..7); -- 16#1AA#
  begin
    j:= 0;
    for i in s'Range loop
      p:= Character'Pos(s(i));
      if p <= 32 then
        Ada.Integer_Text_IO.Put(hex,p + 16#100#,16);  --  + 16#100#: ensure both digits!
        r(j+1..j+3):= '%' & hex(5..6);
        j:= j + 3;
      else
        r(j+1):= s(i);
        j:= j + 1;
      end if;
    end loop;
    return r(1..j);
  end To_URL_Encoding;

  -----------
  -- Start --
  -----------

  procedure Start (
    File       : in String;
    Parameter  : in String  := "";
    Minimized  : in Boolean := False
  )
  is

    C_Operation  : aliased Interfaces.C.char_array :=
                           Interfaces.C.To_C("open");
    C_Executable : aliased Interfaces.C.char_array :=
                           Interfaces.C.To_C(File);
    C_Parameter  : aliased Interfaces.C.char_array :=
                           Interfaces.C.To_C(Parameter);
    -- Parts from Win32Ada:
    subtype PVOID is System.Address;
    subtype HANDLE is PVOID;                    --  winnt.h :144
    subtype HWND is HANDLE;                     --  windef.h :178
    subtype HINSTANCE is HANDLE;
    subtype INT is Interfaces.C.int;                  --  windef.h
    --
    Exe : HINSTANCE;
    pragma Warnings(Off, Exe);
    SW_ShowNormal    : constant := 1;
    SW_ShowMinimized : constant := 2;
    sw: constant array( Boolean ) of INT:=
      (SW_ShowNormal,
       SW_ShowMinimized);
    function GetFocus return HWND;              --  winuser.h:2939
    pragma Import (Stdcall, GetFocus, "GetFocus");
    subtype CHAR is Interfaces.C.char;
    type PCCH is access constant CHAR;
    type PCHAR is access all CHAR;
    subtype LPCSTR is PCCH;
    subtype LPSTR is PCHAR;
    function ShellExecuteA
      (hwnd0 : HWND;
       lpOperation : LPCSTR;
       lpFile : LPCSTR;
       lpParameters : LPSTR;
       lpDirectory : LPCSTR;
       nShowCmd : INT)
      return HINSTANCE;               --  shellapi.h:54
    pragma Import (Stdcall, ShellExecuteA, "ShellExecuteA");   --  shellapi.h:54
    function ShellExecute
      (hwnd0 : HWND;
       lpOperation : LPCSTR;
       lpFile : LPCSTR;
       lpParameters : LPSTR;
       lpDirectory : LPCSTR;
       nShowCmd : INT)
      return HINSTANCE
      renames ShellExecuteA;                       --  shellapi.h:54
  begin
    Exe := ShellExecute
     (hwnd0        => GetFocus,
      lpOperation  => C_Operation (C_Operation'First)'Unchecked_Access,
      lpFile       => C_Executable(C_Executable'First)'Unchecked_Access,
      lpParameters => C_Parameter (C_Parameter'First)'Unchecked_Access,
      lpDirectory  => null,
      nShowCmd     => sw(Minimized));
  end Start;

  -- EXEC improved by :
  --  Martin C. Carlisle, Asst Prof of Comp Sci,
  --  US Air Force Academy, mcc@cs.usafa.af.mil

  procedure Exec(name: String; param:String:="") is
    num_params : Integer := 1;
    use GNAT.OS_Lib;
  begin
    for i in param'Range loop
      if param(i) = ' ' then
        num_params := num_params + 1;
      end if;
    end loop;
    declare
      a_param_list : Argument_List(1..num_params);
      last_start   : Integer := param'First;
      counter      : Integer := 1;
      ok           : Boolean;
    begin
      for i in param'Range loop
        if param(i) = ' ' then
          a_param_list(counter) := new String'(param(last_start..i-1));
          last_start := i+1;
          counter := counter + 1;
        end if;
      end loop;
      a_param_list(num_params) :=
        new String'(param(last_start..param'Last));
      Spawn(name, a_param_list, ok);
      if not ok then
        raise Exec_failed with name & " " & param;
      end if;
    end;
  end Exec;

  procedure Exec_Command (the_command : String) is
    use Ada.Environment_Variables;
  begin
    Exec( Value ("ComSpec")     --  E.g. "C:\WINDOWS\system32\cmd.exe"
          , "/C " & the_command);
  end Exec_Command;

  procedure Create_Desktop_Shortcut (
    Link_Name   : String;  --  Name without extension, e.g. "AZip".
    Target_Path : String;  --  Full path. E.g. Ada.Command_Line.Command_Name
    All_Users   : Boolean  --  When False, shortcut is created only on current user's desktop.
  )
  is
    use Ada.Environment_Variables, Ada.Text_IO;
    --  Code inspired by:
    --  https://superuser.com/questions/455364/how-to-create-a-shortcut-using-a-batch-script
    SCRIPT : constant String := Value ("TEMP") & "\make_shortcut_from_ada.vbs";
    f : File_Type;
    function User return String is
      (if All_Users then Value ("PUBLIC") else Value ("USERPROFILE"));
  begin
    Create (f, Out_File, SCRIPT);
    Put_Line (f, "Set oWS = WScript.CreateObject(""WScript.Shell"")");
    Put_Line (f, "sLinkFile = """ & User & "\Desktop\" & Link_Name & ".lnk""");
    Put_Line (f, "Set oLink = oWS.CreateShortcut(sLinkFile)");
    Put_Line (f, "oLink.TargetPath = """ & Target_Path & '"');
    Put_Line (f, "oLink.Save");
    Close (f);
    --
    Exec_Command ("cscript /nologo " & SCRIPT);
    --
    Open (f, In_File, SCRIPT);
    Delete (f);
  end Create_Desktop_Shortcut;

  procedure Get_Windows_version(
    major, minor: out Integer;
    family      : out Windows_family
  )
  is
    -- Parts from Win32Ada:
    subtype ULONG is Interfaces.C.unsigned_long;      --  windef.h
    subtype INT is Interfaces.C.int;                  --  windef.h
    subtype DWORD is ULONG;                           --  windef.h
    type  BOOL  is new INT;                           --  windef.h
    subtype CHAR is Interfaces.C.char;                --  winnt.h
    type CHAR_Array is array (Natural range <>) of aliased CHAR;
    type OSVERSIONINFOA is                                  --  winbase.h :6649
      record
         dwOSVersionInfoSize : DWORD;               --  winbase.h :6650
         dwMajorVersion : DWORD;               --  winbase.h :6651
         dwMinorVersion : DWORD;               --  winbase.h :6652
         dwBuildNumber : DWORD;               --  winbase.h :6653
         dwPlatformId : DWORD;               --  winbase.h :6654
         szCSDVersion : CHAR_Array (0 .. 127);  --  winbase.h :6655
      end record;
    subtype OSVERSIONINFO is OSVERSIONINFOA;                --  winbase.h :6670
    type LPOSVERSIONINFOA is access all OSVERSIONINFOA;     --  winbase.h :6656
    function GetVersionExA (lpVersionInformation : LPOSVERSIONINFOA)
                           return BOOL;                --  winbase.h :6686
    pragma Import (Stdcall, GetVersionExA, "GetVersionExA"); --  winbase.h :6686

    function GetVersionEx (lpVersionInformation : LPOSVERSIONINFOA)
                          return BOOL renames GetVersionExA;
    -- ^^^^ Parts from Win32Ada
    res:  BOOL;
    info: aliased OSVERSIONINFO;
  begin
    info.dwOSVersionInfoSize:= DWORD(info'Size / 8);
    res:= GetVersionEx(info'Access);
    if res = 1 then
      major:= Integer( info.dwMajorVersion );
      minor:= Integer( info.dwMinorVersion );
      family:= Windows_family'Val(info.dwPlatformId);
    else
      raise cannot_get_Windows_version;
    end if;
  end Get_Windows_version;

  function Temp_dir return String is
    use GNAT.OS_Lib;
    tempdir: constant String_Access:= Getenv ("temp");
  begin
    if tempdir = null or else tempdir.all'Length < 2 then
      return "";
    else
      return tempdir.all & '\';
    end if;
  end Temp_dir;

  function Minimized(Window: GWindows.Base.Base_Window_Type'Class)
    return Boolean
  is
  begin
    return GWindows.Base.Left(Window) <= -32000;
  end Minimized;

  function Valid_Left_Top(Left, Top: Integer)
    return Boolean
  is
    use GWindows.Application;
  begin
    return Left in -320 .. Desktop_Width  - 30 and
           Top  in -320 .. Desktop_Height - 80;
  end Valid_Left_Top;

  -- GdM 22-Feb-2003
  function Find_short_path_name( long: String ) return String is
    -- Parts from Win32Ada:
    subtype ULONG is Interfaces.C.unsigned_long;      --  windef.h
    subtype DWORD is ULONG;                           --  windef.h
    subtype CHAR is Interfaces.C.char;                --  winnt.h
    type PCCH is access constant CHAR;
    type PCHAR is access all CHAR;
    subtype LPCSTR is PCCH;
    subtype LPSTR is PCHAR;
    function GetShortPathNameA (lpszLongPath : LPCSTR;
                                lpszShortPath : LPSTR;
                                cchBuffer : DWORD)
                               return DWORD;
    --  winbase.h :1417
    pragma Import (Stdcall, GetShortPathNameA, "GetShortPathNameA");
    --  winbase.h :1417
    function GetShortPathName (lpszLongPath : LPCSTR;
                               lpszShortPath : LPSTR;
                               cchBuffer : DWORD)
                              return DWORD
    renames GetShortPathNameA;
    -- ^^^^ Parts from Win32Ada
    ls: Integer;
    -- mcc : 18-Mar-2005
    -- short path name may be longer than long
    short_CH : array(0..255) of aliased CHAR :=
      (others => CHAR'First);
    long_CH : array(0..long'Length) of aliased CHAR :=
      (others => CHAR'First);
    short: String(1..long'Length*2);
  begin
    for i in long'Range loop
      long_CH(i-long'First):= CHAR(long(i));
    end loop;
    ls:= Integer(GetShortPathName(
           long_CH(0)'Unchecked_Access,
           short_CH(0)'Unchecked_Access,
           DWORD(short_CH'Last+1))); -- 18-Mar-2005:  alf (add +1)
    for i in 1..ls loop
      short(i):= Character(short_CH(i-1));
    end loop;
    return short(1..ls);
  exception
    when others =>
      return long;
  end Find_short_path_name;

  ----------------------------
  -- Tabs - Property sheets --
  ----------------------------

  package body Property_Tabs_Package is
    --
    tabs: GWindows.Common_Controls.Tab_Window_Control_Type;
    --
    procedure Create(Parent: in out GWindows.Base.Base_Window_Type'Class) is
      margin : constant := 6;
    begin
      -- 1/ Create the tabs holder
      tabs.Create (Parent,
        margin,
        margin,
        Parent.Client_Area_Width - margin * 2,
        Parent.Client_Area_Height - 30 - margin * 2
      );
      tabs.Set_As_Control_Parent; -- <- Avoid button press hanging the app.
      -- 2/ Create each tab
      for s in Tab_enumeration loop
        tabs.Insert_Tab (Tab_enumeration'Pos(s)-Tab_enumeration'Pos(Tab_enumeration'First), Title(s));
        GWindows.Windows.Create_As_Control (
          tab(s), tabs, "",
          0, 0,
          tabs.Client_Area_Width,
          tabs.Client_Area_Height,
          Show => False
        );
        --  Link (tab(s), Handle(tab(s)), False, Control_Link);
        --  -- ^ for buttons from a resource file (André van Splunter)
        tabs.Tab_Window (Tab_enumeration'Pos(s)-Tab_enumeration'Pos(Tab_enumeration'First), tab(s)'Unrestricted_Access);
      end loop;
      -- 3/ Create OK, Cancel buttons:
      Create (ok, Parent, ok_message,
              Parent.Client_Area_Width - 162,
              Parent.Client_Area_Height - 30, 75, 23,
              ID => GWindows.Constants.IDOK
      );
      Create (cancel, Parent, cancel_message,
              Parent.Client_Area_Width - 81,
              Parent.Client_Area_Height - 30, 75, 23,
              ID => GWindows.Constants.IDCANCEL
      );
    end Create;
  end Property_Tabs_Package;

  function "*"
    (Left  : Natural;
     Right : GString) return GString
  is
     Result : GString (1 .. Left * Right'Length);
     Ptr    : Integer := 1;
   begin
     for J in 1 .. Left loop
        Result (Ptr .. Ptr + Right'Length - 1) := Right;
        Ptr := Ptr + Right'Length;
     end loop;
     return Result;
  end "*";

  overriding procedure Create
     (Window     : in out Splitter_with_dashes;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Location   : in     GWindows.Base.Dock_Type;
      Text       : in     GString                              := "";
      Left       : in     Integer                              := 0;
      Top        : in     Integer                              := 0;
      Width      : in     Integer                              := 3;
      Height     : in     Integer                              := 3;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False)
  is
    use GWindows.Base;
  begin
    --  Call parent method:
    GWindows.GControls.GSize_Bars.GSize_Bar_Type (Window).Create (
      Parent, Location, Text, Left, Top, Width, Height, Show, Is_Dynamic);
    --  Add our goodies to make the splitter visible:
    Window.Dashes.Create (
      Window,
      Alignment => GWindows.Static_Controls.Center,
      --  A cheap grip design for the split bar...
      Text =>  (if Location in At_Top | At_Bottom then 30 * "| " else 1000 * ". ")
    );
    Window.Dashes.Dock (Fill);
    Window.Dashes.Enabled (False);  --  Just give a grey look...
  end Create;

begin
  Common_Fonts.Create_Common_Fonts;
end GWin_Util;
