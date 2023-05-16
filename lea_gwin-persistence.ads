-----------------------------------------------------------------------------------
--  Persistence using either the registry or a configuration file, if available  --
-----------------------------------------------------------------------------------

with LEA_Common.User_options;

package LEA_GWin.Persistence is

  function Cfg_file_available return Boolean;
  --  ^ When True, we are in Stealth Mode and don't want to
  --    leave any trace in the registry!

  procedure Load (opt : out LEA_Common.User_options.Option_Pack_Type);
  procedure Save (opt : in  LEA_Common.User_options.Option_Pack_Type);

end LEA_GWin.Persistence;
