with Interfaces.C; use Interfaces.C;
with Ada.Text_IO;  use Ada.Text_IO;

procedure Show_C_Func is

   function my_func (a : int) return int with
      Import     => True,
      Convention => C;

   V : int;

   func_cnt : int with
      Import     => True,
      Convention => C;
      --  We can access the func_cnt variable
      --  from test.c

begin
   V := my_func (1);
   V := my_func (2);
   V := my_func (3);
   Put_Line ("Result is " & int'Image (V));

   Put_Line ("Function was called " & int'Image (func_cnt) & " times");
end Show_C_Func;
