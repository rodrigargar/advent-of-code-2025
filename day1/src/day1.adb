with Ada.Text_IO; use Ada.Text_IO;

procedure Day1 is
   type Dial_Position is mod 100;
   Dial : Dial_Position := 50;
   Password_1 : Natural := 0;
   Password_2 : Natural := 0;

   procedure Turn_Dial (Rotation : String) is
      Direction : constant Character := Rotation (Rotation'First);
      Quantity : constant Positive := Positive'Value
         (Rotation (Rotation'First + 1 .. Rotation'Last));
      Full_Turns : constant Natural := Quantity / Dial_Position'Modulus;
      Effective : constant Dial_Position := Dial_Position'Mod (Quantity);
   begin
      Password_2 := Password_2 + Full_Turns;
      if Direction = 'L' then
         if (Dial /= 0) and then (Dial <= Effective) then
            Password_2 := Password_2 + 1;
         end if;
         Dial := Dial - Effective;
      elsif Direction = 'R' then
         if (Dial /= 0) and then ((Dial_Position'Last - Dial) < Effective) then
            Password_2 := Password_2 + 1;
         end if;
         Dial := Dial + Effective;
      end if;
   end Turn_Dial;

   Document : File_Type;
   Doc_Name : constant String := "document.txt";
begin
   Open (Document, In_File, Doc_Name);
   while not End_Of_File (Document) loop
      declare
         Doc_Line : constant String := Get_Line (Document);
      begin
         Turn_Dial (Doc_Line);
         Put_Line ("Dial position after rotation " &
            Doc_Line & ": " & Dial_Position'Image (Dial));
         if Dial = 0 then
            Password_1 := Password_1 + 1;
         end if;
      end;
   end loop;
   Close (Document);
   Put_Line ("The password for part 1 is " & Natural'Image (Password_1));
   Put_Line ("The password for part 2 is " & Natural'Image (Password_2));
end Day1;
