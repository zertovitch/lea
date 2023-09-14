--  Time_display returns date & time, current or given.
--  E.g.: "2023/08/01  05:49:51.457"
--  Useful for a log file or for displaying a lengthy operation.
--
--  Test program in following comment:
--
--    with Ada.Text_IO, Time_display;
--    procedure Test is begin Ada.Text_IO.Put(Time_display);end;

with Ada.Calendar;

function Time_Display
  (T            : Ada.Calendar.Time := Ada.Calendar.Clock;
   Intra_Day    : Boolean           := True;
   Seconds      : Boolean           := True;
   Milliseconds : Boolean           := True)
  return String
is
  use Ada.Calendar;
  sdu : constant Day_Duration := Ada.Calendar.Seconds (T);
  type Sec_Int is range 0 .. 86_400;
  s : constant Sec_Int := Sec_Int (sdu - 0.5);
  m : constant Sec_Int := s / 60;
  --  + 100: trick for obtaining 0x
  sY    : constant String := Integer'Image  (Year (T));
  sM    : constant String := Integer'Image (Month (T) + 100);
  sD    : constant String := Integer'Image   (Day (T) + 100);
  shour : constant String := Sec_Int'Image  (m  /  60 + 100);
  smin  : constant String := Sec_Int'Image  (m mod 60 + 100);
  ssec  : constant String := Sec_Int'Image  (s mod 60 + 100);
  sfrac : constant String := Duration'Image (sdu - Duration (s));
  --
  function Optional_Milliseconds return String is
  begin
    if Milliseconds then
      return sfrac (sfrac'First + 2 .. sfrac'First + 5);
    else
      return "";
    end if;
  end Optional_Milliseconds;
  --
  function Optional_Seconds return String is
  begin
    if Seconds then
      return
        ':' & ssec (ssec'Last - 1 .. ssec'Last) &
        Optional_Milliseconds;
    else
      return "";
    end if;
  end Optional_Seconds;
  --
  function Optional_Intra_Day return String is
  begin
    if Intra_Day then
      return
        "  " &
        shour (shour'Last - 1 .. shour'Last) & ':' &
        smin (smin'Last - 1 .. smin'Last) & Optional_Seconds;
    else
      return "";
    end if;
  end Optional_Intra_Day;

begin
  return
    sY (sY'Last - 3 .. sY'Last) & '/' &   --  Not Year 10'000 compliant.
    sM (sM'Last - 1 .. sM'Last) & '/' &
    sD (sD'Last - 1 .. sD'Last) &
    Optional_Intra_Day;
end Time_Display;
