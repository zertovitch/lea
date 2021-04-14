with GWindows.Types, Interfaces.C;

package body Windows_Timers is

  use GWindows.Base, Interfaces.C;

  --  Windows interfacing part.
  --  Functionalities from Windows API's Winuser.

  type TIMERPROC is access procedure (hwnd    : GWindows.Types.Handle;
                                      uMsg    : Interfaces.C.unsigned;
                                      idEvent : Interfaces.C.unsigned;
                                      dwTime  : Interfaces.C.unsigned_long);
  pragma Convention (Stdcall, TIMERPROC);

  function SetTimer (hWnd        : GWindows.Types.Handle;
                     nIDEvent    : Interfaces.C.unsigned;
                     uElapse     : Interfaces.C.unsigned;
                     lpTimerFunc : TIMERPROC)
                    return Interfaces.C.unsigned;

  function KillTimer (hWnd     : GWindows.Types.Handle;
                      uIDEvent : Interfaces.C.unsigned)
                     return Interfaces.C.int;

  pragma Import (Stdcall, SetTimer, "SetTimer");
  pragma Import (Stdcall, KillTimer, "KillTimer");

  ---------------
  -- Set_Timer --
  ---------------

  procedure Set_Timer(Window       : GWindows.Base.Base_Window_Type'Class;
                      ID_Event     : Natural;
                      Milliseconds : Natural)
  is
    res: Interfaces.C.unsigned;
  begin
    res:= SetTimer(hWnd        => Handle(Window),
                   nIDEvent    => Interfaces.C.unsigned(ID_Event),
                   uElapse     => Interfaces.C.unsigned(Milliseconds),
                   lpTimerFunc => null
                  );
    if res = 0 then
      raise error;
    end if;
  end Set_Timer;

  ----------------
  -- Kill_Timer --
  ----------------

  procedure Kill_Timer(Window   : GWindows.Base.Base_Window_Type'Class;
                       ID_Event : Natural)
  is
    res: Interfaces.C.int;
  begin
    res:= KillTimer(hWnd     => Handle(Window),
                    uIDEvent => Interfaces.C.unsigned(ID_Event)
                  );
    if res = 0 then
      raise error;
    end if;
  end Kill_Timer;

end Windows_Timers;
