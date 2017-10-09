package body LEA_Common.User_options is

  package body Persistence is

    type Key is
      ( view_mode, color_theme,
        backup,
        win_left, win_top, win_width, win_height,
        maximized, children_maximized,
        tree_portion,
        mru1, mru2, mru3, mru4, mru5, mru6, mru7, mru8, mru9
      );

    pragma Unreferenced (mru2, mru3, mru4, mru5, mru6, mru7, mru8);

    procedure Load(opt: out Option_Pack_Type) is
      --  !! (useless) defaults: Option_Pack_Type;
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
              when color_theme =>
                opt.color_theme:= Color_Theme_Type'Wide_Value(s);
              when backup =>
                opt.backup := Backup_mode'Wide_Value(s);
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
              when mru1..mru9 =>
                opt.mru( Key'Pos(k)-Key'Pos(mru1)+1 ):=
                  To_Unbounded_Wide_String(s);
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

    procedure Save(opt: in Option_Pack_Type) is
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
            when color_theme =>
              R(Color_Theme_Type'Wide_Image(opt.color_theme));
            when backup =>
              R(Backup_mode'Wide_Image(opt.backup));
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
            when mru1..mru9 =>
              R( To_Wide_String(opt.mru( Key'Pos(k)-Key'Pos(mru1)+1 )) );
          end case;
        end;
      end loop;
    end Save;

  end Persistence;

end LEA_Common.User_options;
