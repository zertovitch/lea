with Config, GWindows.Registry, Zip;

with Ada.Command_Line,
     Ada.IO_Exceptions,
     Ada.Text_IO;

package body LEA_GWin.Persistence is

  ----------------------------------------------
  --  Persistence using the Windows Registry  --
  ----------------------------------------------

  kname : constant GString := "Software\LEA";
  use GWindows.Registry;

  function Read_reg_key (topic : Wide_String) return Wide_String is
  begin
    return Get_Value (kname, topic, HKEY_CURRENT_USER);
  end Read_reg_key;

  procedure Write_reg_key (topic : Wide_String; value : Wide_String) is
  begin
    Register (kname, topic, value, HKEY_CURRENT_USER);
  end Write_reg_key;

  package Registry_persistence is new
    LEA_Common.User_options.Persistence (Read_reg_key, Write_reg_key);

  ----------------------------------------------
  --  Persistence using a configuration file  --
  ----------------------------------------------

  function Config_name return String is
    full : constant String := Ada.Command_Line.Command_Name;
    last : Natural := full'First - 1;
  begin
    for i in full'Range loop
      if full (i) = '\' or full (i) = '/' then
        last := i;
      end if;
    end loop;
    return full (full'First .. last) & "lea.cfg";
  end Config_name;

  app_section : constant String := "LEA user options";

  procedure Create_new_config;

  function Read_cfg_key (topic : Wide_String) return Wide_String is
    cfg : Config.Configuration;
  begin
    cfg.Init (Config_name);
    return To_GString_From_String (cfg.Value_Of ("*", To_String (topic)));
  end Read_cfg_key;

  procedure Write_cfg_key (topic : Wide_String; value : Wide_String) is
    cfg : Config.Configuration;
  begin
    cfg.Init (Config_name);
    for attempt in 1 .. 2 loop
      begin
        cfg.Replace_Value (app_section, To_String (topic), To_String (value));
      exception
        when Config.Location_Not_Found =>
          Create_new_config;
      end;
    end loop;
  exception
    when Ada.IO_Exceptions.Use_Error =>  --  Read-only
      null;  --  Do nothing, lea.exe and lea.cfg may be on a read-only device.
  end Write_cfg_key;

  package Configuration_persistence is new
    LEA_Common.User_options.Persistence (Read_cfg_key, Write_cfg_key);

  procedure Create_new_config is
    use Ada.Text_IO;
    nf : File_Type;
    procedure Config_Output (key_name : String) is
    begin
      Put_Line (nf, key_name & '=');
    end Config_Output;
    procedure Output_Empty_Keys is new LEA_Common.User_options.Show_Persistence_Keys (Config_Output);
  begin
    Create (nf, Out_File, Config_name);
    Put_Line (nf, ";  This is the configuration file for LEA, in the ""no trace in registry"" mode.");
    Put_Line (nf, ";  Delete this file for using the registry again.");
    Put_Line (nf, ";  NB: the settings are the same for all users.");
    Put_Line (nf, ";  LEA Web site: https://l-e-a.sourceforge.io/");
    Put_Line (nf, ";");
    Put_Line (nf, '[' & app_section & ']');
    Output_Empty_Keys;
    Close (nf);
  end Create_new_config;

  ---------------------------------------------------------------------
  --  Persistence using either the registry or a configuration file  --
  ---------------------------------------------------------------------

  function Cfg_file_available return Boolean is
  begin
    return Zip.Exists (Config_name);
  end Cfg_file_available;

  procedure Load (opt : out LEA_Common.User_options.Option_Pack_Type) is
  begin
    if Cfg_file_available then
      Configuration_persistence.Load (opt);
    else
      Registry_persistence.Load (opt);
    end if;
  end Load;

  procedure Save (opt : in  LEA_Common.User_options.Option_Pack_Type) is
  begin
    if Cfg_file_available then
      Configuration_persistence.Save (opt);
    else
      Registry_persistence.Save (opt);
    end if;
  end Save;

end LEA_GWin.Persistence;
