-----------------------------------------------------------------------------------
--  Persistence using either the registry or a configuration file, if available  --
-----------------------------------------------------------------------------------

with LEA_Common.User_options;

with GWindows.Persistence_IO;

package LEA_GWin.Persistence is

  package Key_IO is
    new GWindows.Persistence_IO
      (app_display_name => "LEA",
       app_file_name    => "lea",
       app_url          => LEA_Common.LEA_web_page,
       Persistence_Key  => LEA_Common.User_options.Persistence_Key);

  package Blockwise_IO is
    new LEA_Common.User_options.Persistence
      (Read_Key  => Key_IO.Read_Key,
       Write_Key => Key_IO.Write_Key);

end LEA_GWin.Persistence;
