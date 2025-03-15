with Ada.Strings.Wide_Fixed;

package body LEA_Common.User_options is

  procedure Toggle_show_special (o : in out Option_Pack_Type) is
  begin
    if o.show_special = Show_special_symbol_mode'Last then
      o.show_special := Show_special_symbol_mode'First;
    else
      o.show_special := Show_special_symbol_mode'Succ (o.show_special);
    end if;
  end Toggle_show_special;

  package body Persistence is

    sep : constant UTF_16_String := ">";

    procedure Load (opt : out Option_Pack_Type) is
      mru_idx : Positive;
      sep_pos_1 : Natural;
      use Ada.Strings.Wide_Fixed;
    begin
      for k in Persistence_Key loop
        begin
          declare
            s : constant Wide_String := Read_Key (k);
          begin
            case k is
              when view_mode =>
                opt.view_mode := View_Mode_Type'Wide_Value (s);
              when toolset_mode =>
                opt.toolset := Toolset_mode_type'Wide_Value (s);
              when color_theme =>
                opt.color_theme := Color_Themes.Color_Theme_Type'Wide_Value (s);
              when backup =>
                opt.backup := Backup_mode'Wide_Value (s);
              when indent =>
                opt.indentation := Integer'Wide_Value (s);
              when tab_width =>
                opt.tab_width := Integer'Wide_Value (s);
              when edge =>
                opt.right_margin := Integer'Wide_Value (s);
              when show_special =>
                opt.show_special := Show_special_symbol_mode'Wide_Value (s);
              when show_indent =>
                opt.show_indent := Boolean'Wide_Value (s);
              when auto_insert =>
                opt.auto_insert := Boolean'Wide_Value (s);
              when win_left =>
                opt.win_left := Integer'Wide_Value (s);
              when win_top =>
                opt.win_top := Integer'Wide_Value (s);
              when win_width =>
                opt.win_width := Integer'Wide_Value (s);
              when win_height =>
                opt.win_height := Integer'Wide_Value (s);
              when maximized =>
                opt.MDI_main_maximized := Boolean'Wide_Value (s);
              when children_maximized =>
                opt.MDI_childen_maximized := Boolean'Wide_Value (s);
              when tree_portion =>
                opt.tree_portion := Float'Wide_Value (s);
              when project_tree_portion =>
                opt.project_tree_portion := Float'Wide_Value (s);
              when message_list_portion =>
                opt.message_list_portion := Float'Wide_Value (s);
              when subprogram_tree_portion =>
                opt.subprogram_tree_portion := Float'Wide_Value (s);
              when mru1 .. mru9 =>
                mru_idx := Persistence_Key'Pos (k) - Persistence_Key'Pos (mru1) + 1;
                sep_pos_1 := Index (s, sep);
                if sep_pos_1 > 0 then  --  Separator between file name and line number
                  opt.mru (mru_idx).name := To_Unbounded_Wide_String (s (s'First .. sep_pos_1 - 1));
                  opt.mru (mru_idx).line := Integer'Wide_Value (s (sep_pos_1 + 1 .. s'Last));
                else
                  opt.mru (mru_idx).name := To_Unbounded_Wide_String (s);
                end if;
              when ada_files_filter =>
                if s /= "" and then
                  s (s'First) = '*'
                  --  ^ Filter registry garbage AND check that first chatacter is a '*'...
                then
                  opt.ada_files_filter := To_Unbounded_Wide_String (s);
                end if;
              when smart_editor =>
                opt.smart_editor := Boolean'Wide_Value (s);
              when level_for_remarks =>
                opt.level_for_remarks := HAC_Sys.Defs.Remark_Level'Wide_Value (s);
            end case;
          end;
        exception
          when others =>
            null;
            --  This key is missing (from registry, config file,...)
            --  Data_Error or something else.
            --  We just continue...
        end;
      end loop;
    end Load;

    procedure Save (opt : in Option_Pack_Type) is
      mru_idx : Positive;
    begin
      for k in Persistence_Key loop
        declare
          procedure R (v : Wide_String) is
          begin
            Write_Key (k, v);
          end R;
        begin
          case k is
            when view_mode =>
              R (opt.view_mode'Wide_Image);
            when toolset_mode =>
              R (opt.toolset'Wide_Image);
            when color_theme =>
              R (opt.color_theme'Wide_Image);
            when backup =>
              R (opt.backup'Wide_Image);
            when indent =>
              R (opt.indentation'Wide_Image);
            when tab_width =>
              R (opt.tab_width'Wide_Image);
            when edge =>
              R (opt.right_margin'Wide_Image);
            when show_special =>
              R (opt.show_special'Wide_Image);
            when show_indent =>
              R (opt.show_indent'Wide_Image);
            when auto_insert =>
              R (opt.auto_insert'Wide_Image);
            when win_left =>
              R (opt.win_left'Wide_Image);
            when win_top =>
              R (opt.win_top'Wide_Image);
            when win_width =>
              R (opt.win_width'Wide_Image);
            when win_height =>
              R (opt.win_height'Wide_Image);
            when maximized =>
              R (opt.MDI_main_maximized'Wide_Image);
            when children_maximized =>
              R (opt.MDI_childen_maximized'Wide_Image);
            when tree_portion =>
              R (opt.tree_portion'Wide_Image);
            when project_tree_portion =>
              R (opt.tree_portion'Wide_Image);
            when message_list_portion =>
              R (opt.message_list_portion'Wide_Image);
            when subprogram_tree_portion =>
              R (opt.subprogram_tree_portion'Wide_Image);
            when mru1 .. mru9 =>
              mru_idx := Persistence_Key'Pos (k) - Persistence_Key'Pos (mru1) + 1;
              R (To_Wide_String (opt.mru (mru_idx).name) & sep &
                 opt.mru (mru_idx).line'Wide_Image);
            when ada_files_filter =>
              R (To_Wide_String (opt.ada_files_filter));
            when smart_editor =>
              R (opt.smart_editor'Wide_Image);
            when level_for_remarks =>
              R (opt.level_for_remarks'Wide_Image);
          end case;
        end;
      end loop;
    end Save;

  end Persistence;

  procedure Show_Persistence_Keys is
  begin
    for k in Persistence_Key loop
      String_Output (k'Image);
    end loop;
  end Show_Persistence_Keys;

end LEA_Common.User_options;
