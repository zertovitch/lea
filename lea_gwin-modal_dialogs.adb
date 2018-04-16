--  Small modal dialogs

with LEA_Common;                        use LEA_Common;

with LEA_Resource_GUI;                  use LEA_Resource_GUI;

with GWindows.Application;              use GWindows.Application;
with GWindows.Base;                     use GWindows.Base;
with GWindows.Constants;                use GWindows.Constants;
with GWindows.Message_Boxes;            use GWindows.Message_Boxes;
with GWindows.Static_Controls.Web;      use GWindows.Static_Controls.Web;

with GNAT.Compiler_Version;

package body LEA_GWin.Modal_dialogs is

  procedure Do_go_to_line (MDI_Child: in out MDI_Child_Type) is
    box : Go_to_line_box_Type;
    new_line: Integer := 0;
    --
    procedure Get_Data (Window : in out GWindows.Base.Base_Window_Type'Class) is
    begin
      new_line:= Integer'Wide_Value(box.Line_value_box.Text);
    exception
      when others =>
        Message_Box(Window, "Invalid data", "Line number is invalid", OK_Box, Error_Icon);
        new_line := 0;
    end Get_Data;
    --
  begin
    box.Create_Full_Dialog(MDI_Child);
    box.Center(MDI_Child);
    box.Line_value_box.Text (Integer'Wide_Image (MDI_Child.Editor.Get_current_line + 1));
    box.Line_value_box.Set_Selection (0, 10);
    box.Line_value_box.Focus;
    On_Destroy_Handler (box, Get_Data'Unrestricted_Access);
    case Show_Dialog (box, MDI_Child) is
      when IDOK     =>
        if new_line > 0 then
          MDI_Child.Editor.Set_current_line (new_line - 1);
        end if;
      when others   =>
        null;  --  Contains the IDCANCEL case
    end case;
  end Do_go_to_line;

  procedure Do_about (MDI_Main : in out MDI_Main_Type) is
    box: About_box_Type;
    url_lea, url_gnat, url_gnavi, url_hac, url_resedit: URL_Type;
    --
    function GNAT_Version_string return String is
      package CVer is new GNAT.Compiler_Version;
      v: constant String:= CVer.Version;
    begin
      if v(v'First..v'First+2) = "GPL" then
        return v;
      else
        return "GMGPL " & v & " (TDM-GCC / MinGW)";
      end if;
    end GNAT_Version_string;
    --
  begin
    box.Create_Full_Dialog(MDI_Main);
    box.Copyright_label.Text(S2G(Version_info.LegalCopyright));
    box.Version_label.Text(S2G(Version_info.FileVersion));
    Create_and_Swap (url_lea,     box.AZip_URL,    box, S2G(LEA_web_page));
    Create_and_Swap (url_gnat,    box.GNAT_URL,    box, "http://libre.adacore.com");
    box.GNAT_Version.Text (S2G("version " & GNAT_Version_string));
    Create_and_Swap (url_gnavi,   box.GNAVI_URL,   box, "http://sf.net/projects/gnavi");
    Create_and_Swap (url_hac,     box.HAC_URL,     box, "http://sf.net/projects/hacadacompiler");
    Create_and_Swap (url_resedit, box.ResEdit_URL, box, "http://resedit.net");
    box.Center;
    if Show_Dialog (box, MDI_Main) = IDOK then
      null;
    end if;
  end Do_about;

end LEA_GWin.Modal_dialogs;
