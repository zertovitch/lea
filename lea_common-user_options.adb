with Ada.Strings.Wide_Fixed;            use Ada.Strings.Wide_Fixed;

package body LEA_Common.User_options is

  procedure Toggle_show_special (o: in out Option_Pack_Type) is
  begin
    if o.show_special = Show_special_symbol_mode'Last then
      o.show_special := Show_special_symbol_mode'First;
    else
      o.show_special := Show_special_symbol_mode'Succ(o.show_special);
    end if;
  end;

  package body Persistence is

    type Key is
      ( view_mode,      --  Notepad, Studio
        toolset_mode,   --  HAC, GNAT, ...
        color_theme,
        backup,
        indent, tab_width,
        edge,  --  right margin
        show_special,
        win_left, win_top, win_width, win_height,
        maximized, children_maximized,
        tree_portion,  --  !! to be removed
        project_tree_portion,
        message_list_portion,
        subprogram_tree_portion,
        mru1, mru2, mru3, mru4, mru5, mru6, mru7, mru8, mru9,
        ada_files_filter
      );

    pragma Unreferenced (mru2, mru3, mru4, mru5, mru6, mru7, mru8);

    sep: constant UTF_16_String:= ">";

    procedure Load (opt: out Option_Pack_Type) is
      mru_idx: Positive;
      sep_pos_1: Natural;
    begin
      for k in Key loop
        begin
          declare
            ks: constant Wide_String:= Key'Wide_Image(k);
            s : constant Wide_String:= Read_key(ks);
          begin
            case k is
              when view_mode =>
                opt.view_mode:= View_Mode_Type'Wide_Value(s);
              when toolset_mode =>
                opt.toolset:= Toolset_mode_type'Wide_Value(s);
              when color_theme =>
                opt.color_theme:= Color_Theme_Type'Wide_Value(s);
              when backup =>
                opt.backup := Backup_mode'Wide_Value(s);
              when indent =>
                opt.indentation := Integer'Wide_Value(s);
              when tab_width =>
                opt.tab_width := Integer'Wide_Value(s);
              when edge =>
                opt.right_margin := Integer'Wide_Value(s);
              when show_special =>
                opt.show_special := Show_special_symbol_mode'Wide_Value(s);
              when win_left =>
                opt.win_left:= Integer'Wide_Value(s);
              when win_top =>
                opt.win_top:= Integer'Wide_Value(s);
              when win_width =>
                opt.win_width:= Integer'Wide_Value(s);
              when win_height =>
                opt.win_height:= Integer'Wide_Value(s);
              when maximized =>
                opt.MDI_main_maximized:= Boolean'Wide_Value(s);
              when children_maximized =>
                opt.MDI_childen_maximized:= Boolean'Wide_Value(s);
              when tree_portion =>
                opt.tree_portion:= Float'Wide_Value(s);
              when project_tree_portion =>
                opt.project_tree_portion:= Float'Wide_Value(s);
              when message_list_portion =>
                opt.message_list_portion:= Float'Wide_Value(s);
              when subprogram_tree_portion =>
                opt.subprogram_tree_portion:= Float'Wide_Value(s);
              when mru1 .. mru9 =>
                mru_idx := Key'Pos(k) - Key'Pos(mru1) + 1;
                sep_pos_1:= Index(s, sep);
                if sep_pos_1 > 0 then  --  Separator between file name and line number
                  opt.mru(mru_idx).name := To_Unbounded_Wide_String(s(s'First .. sep_pos_1-1));
                  opt.mru(mru_idx).line := Integer'Wide_Value(s(sep_pos_1+1 .. s'Last));
                else
                  opt.mru(mru_idx).name := To_Unbounded_Wide_String(s);
                end if;
              when ada_files_filter =>
                if s /= "" and then
                  s (s'First) = '*'
                  --  ^ Filter registry garbage AND check that first chatacter is a '*'...
                then
                  opt.ada_files_filter := To_Unbounded_Wide_String(s);
                end if;
            end case;
          end;
        exception
          when others =>
            null;
            -- This key is missing (from registry, config file,...)
            -- Data_Error or something else.
            -- We just continue...
        end;
      end loop;
    end Load;

    procedure Save (opt: in Option_Pack_Type) is
      mru_idx: Positive;
    begin
      for k in Key loop
        declare
          ks: constant Wide_String:= Key'Wide_Image(k);
          procedure R( v: Wide_String ) is
          begin
            Write_key(ks, v);
          end R;
        begin
          case k is
            when view_mode =>
              R(View_Mode_Type'Wide_Image(opt.view_mode));
            when toolset_mode =>
              R(Toolset_mode_type'Wide_Image(opt.toolset));
            when color_theme =>
              R(Color_Theme_Type'Wide_Image(opt.color_theme));
            when backup =>
              R(Backup_mode'Wide_Image(opt.backup));
            when indent =>
              R(Integer'Wide_Image(opt.indentation));
            when tab_width =>
              R(Integer'Wide_Image(opt.tab_width));
            when edge =>
              R(Integer'Wide_Image(opt.right_margin));
            when show_special =>
              R(Show_special_symbol_mode'Wide_Image(opt.show_special));
            when win_left =>
              R(Integer'Wide_Image(opt.win_left));
            when win_top =>
              R(Integer'Wide_Image(opt.win_top));
            when win_width =>
              R(Integer'Wide_Image(opt.win_width));
            when win_height =>
              R(Integer'Wide_Image(opt.win_height));
            when maximized =>
              R(Boolean'Wide_Image(opt.MDI_main_maximized));
            when children_maximized =>
              R(Boolean'Wide_Image(opt.MDI_childen_maximized));
            when tree_portion =>
              R(Float'Wide_Image(opt.tree_portion));
            when project_tree_portion =>
              R(Float'Wide_Image(opt.tree_portion));
            when message_list_portion =>
              R(Float'Wide_Image(opt.message_list_portion));
            when subprogram_tree_portion =>
              R(Float'Wide_Image(opt.subprogram_tree_portion));
            when mru1 .. mru9 =>
              mru_idx := Key'Pos(k) - Key'Pos(mru1) + 1;
              R( To_Wide_String(opt.mru(mru_idx).name) & sep &
                 Integer'Wide_Image(opt.mru(mru_idx).line) );
            when ada_files_filter =>
              R (To_Wide_String(opt.ada_files_filter));
          end case;
        end;
      end loop;
    end Save;

  end Persistence;

end LEA_Common.User_options;
