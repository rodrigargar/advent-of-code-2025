with Ada.Text_IO; use Ada.Text_IO;

procedure Day3 is
   Battery_Banks : File_Type;
   Banks_File_Name : constant String := "test.txt";
   Sum_Joltage : Natural := 0;
   Ultra_Sum : Long_Integer := 0;
begin
   Open (Battery_Banks, In_File, Banks_File_Name);
   while not End_Of_File (Battery_Banks) loop
      declare
         Bank : constant String := Get_Line (Battery_Banks);

         Max_Joltage : String (1 .. 2) := (others => '0');
         Max_Index : Positive := Bank'First;

         Ultra_Joltage : String (1 .. 12) := (others => '0');
         Ultra_Index : Positive := Bank'First;
      begin
         Put_Line ("For bank " & Bank);
         for C in Max_Joltage'Range loop
            for I in Max_Index .. Bank'Last - Max_Joltage'Last + C loop
               if Bank (I) > Max_Joltage (C) then
                  Max_Joltage (C) := Bank (I);
                  Max_Index := I + 1;
               end if;
            end loop;
         end loop;
         Put_Line ("Found max " & Max_Joltage);
         Sum_Joltage := Sum_Joltage + Natural'Value (Max_Joltage);

         for C in Ultra_Joltage'Range loop
            for I in Ultra_Index .. Bank'Last - Ultra_Joltage'Last + C loop
               if Bank (I) > Ultra_Joltage (C) then
                  Ultra_Joltage (C) := Bank (I);
                  Ultra_Index := I + 1;
               end if;
            end loop;
         end loop;
         Put_Line ("Found ultra " & Ultra_Joltage);
         Ultra_Sum := Ultra_Sum + Long_Integer'Value (Ultra_Joltage);
      end;
   end loop;
   Put_Line ("The sum of all joltages is" & Sum_Joltage'Image);
   Put_Line ("The ultra sum of all joltages is" & Ultra_Sum'Image);
   Close (Battery_Banks);
end Day3;
