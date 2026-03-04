with LEA_GWin.MDI_Child;

package LEA_GWin.Alire is

  procedure Alr_Build (window : in out MDI_Child.MDI_Child_Type);
  procedure Alr_Run (window : in out MDI_Child.MDI_Child_Type);

  function Detect_Alire_TOML (file_name : String) return String;
  --
  --  Input: full name of some file, usually an Ada file.
  --  Output: full name of the alire.toml file located in the same or one of the upper directories, or "" if not found.

end LEA_GWin.Alire;
