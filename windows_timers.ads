-- Set_Timer  makes the system produce a periodic message of value WM_TIMER
--              to the specified window.
-- Kill_Timer removes the timer.

with GWindows.Base;

package Windows_Timers is

  WM_TIMER : constant := 16#113#;

  procedure Set_Timer(Window       : GWindows.Base.Base_Window_Type'Class;
                      ID_Event     : Natural;
                      Milliseconds : Natural);

  procedure Kill_Timer(Window   : GWindows.Base.Base_Window_Type'Class;
                       ID_Event : Natural);

  error: exception;

end Windows_Timers;
