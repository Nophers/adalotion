with Interfaces.C; use Interfaces.C;

procedure Display_C_Struct is

   type c_struct is record
      a : int;
      b : long;
      c : unsigned;
      d : double;
   end record with
      Convention => C;

begin
   null;
end Display_C_Struct;
