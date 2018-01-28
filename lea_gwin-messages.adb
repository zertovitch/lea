with LEA_GWin.MDI_Main;                 use LEA_GWin.MDI_Main;

package body LEA_GWin.Messages is

  overriding procedure On_Click (Control : in out Message_List_Type) is
  begin
    Control.On_Double_Click;
    Control.Focus;
  end On_Click;

  overriding procedure On_Double_Click (Control : in out Message_List_Type) is
    use LEA_LV_Ex;
    pl: Data_Access;
    mm: MDI_Main_Access;
  begin
    for i in 0 .. Control.Item_Count loop
      if Control.Is_Selected (i) then
        pl := Control.Item_Data (i);
        if pl /= null then
          mm := MDI_Main_Access (Control.mdi_main_parent);
          mm.Open_Child_Window_And_Load (pl.file, pl.line, pl.col);
          --  At this point focus is on the editor window.
          exit;
        end if;
      end if;
    end loop;
  end On_Double_Click;

end LEA_GWin.Messages;
