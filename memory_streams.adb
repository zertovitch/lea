
package body Memory_Streams is

  --  *************************************************************************
  overriding procedure Read (Stream : in out Memory_Stream;
                             Item   :    out Stream_Element_Array;
                             Last   :    out Stream_Element_Offset)
  is
    Source : Stream_Element_Array (1 .. Stream.Last_Offset) with Address => Stream.Address;
    pragma Import (Ada, Source);
    --  ^ This pragma prevents an attempt of initialization (possibly brought by
    --    a validity check in Debug mode) and solves following error:
    --        error: invalid address clause for initialized object "Source"
    --        error: reference to variable "Stream" not allowed (RM 13.1(22))
    Next_Offset : constant Stream_Element_Offset := Stream.Current_Offset + Item'Length;
  begin
    Item := Source (Stream.Current_Offset .. Next_Offset - 1);
    Last := Item'Last;
    Stream.Current_Offset := Next_Offset;
  end Read;

  --  *************************************************************************
  overriding procedure Write (Stream : in out Memory_Stream;
                              Item   :        Stream_Element_Array)
  is
    Target : Stream_Element_Array (1 .. Stream.Last_Offset) with Address => Stream.Address;
    pragma Import (Ada, Target);
    Next_Offset : constant Stream_Element_Offset := Stream.Current_Offset + Item'Length;
  begin
    Target (Stream.Current_Offset .. Next_Offset - 1) := Item;
    Stream.Current_Offset := Next_Offset;
  end Write;

  --  *************************************************************************
  procedure Set_Address (Stream  : in out Memory_Stream;
                         Address :        System.Address;
                         Length  :        Natural)
  is
  begin
    Stream.Address        := Address;
    Stream.Last_Offset    := Stream_Element_Offset (Length);
    Stream.Current_Offset := Stream_Element_Offset (1);
  end Set_Address;

begin

  if Stream_Element_Array'Component_Size /= System.Storage_Unit then
    raise Program_Error;
  end if;

end Memory_Streams;
