procedure Display_C_Enum is

   type C_Enum is (A, B, C) with
      Convention => C;
      --  Use C convention for C_Enum
begin
   null;
end Display_C_Enum;
