--
--  Streams writing/reading to/from a bounded memory block
--
--  A stream can't be used simultaneously for writing and reading.
--

with System;
with Ada.Streams;

package Memory_Streams is

   use Ada.Streams;

   Init_Error : exception;

   type Stream_Element_Array_Access is access all Stream_Element_Array;

   type Memory_Stream is new Ada.Streams.Root_Stream_Type with private;

   overriding procedure Read (Stream : in out Memory_Stream;
                              Item   :    out Stream_Element_Array;
                              Last   :    out Stream_Element_Offset);

   overriding procedure Write (Stream : in out Memory_Stream;
                               Item   :        Stream_Element_Array);

   procedure Set_Address (Stream  : in out Memory_Stream;
                          Address :        System.Address;
                          Length  :        Natural);

private

  type Memory_Stream is new Ada.Streams.Root_Stream_Type with record
    Address        : System.Address;
    Last_Offset    : Stream_Element_Offset := 0;
    Current_Offset : Stream_Element_Offset := 1;
  end record;

end Memory_Streams;
