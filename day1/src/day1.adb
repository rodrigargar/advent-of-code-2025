with Ada.Text_IO; use Ada.Text_IO;

procedure Day1 is
   type Dial_Position is mod 100;
   Dial : Dial_Position := 50;
   Password : Natural := 0;
   Document : File_Type;
   Doc_Name : constant String := "document.txt";

   procedure Turn_Dial (Rotation : String) is
      Direction : Character := Rotation (Rotation'First);
      Quantity : Dial_Position :=
         Dial_Position'Value (Rotation (Rotation'First + 1 .. Rotation'Last));
   begin
      if Direction = 'L' then
         Dial := Dial - Quantity;
      elsif Direction = 'R' then
         Dial := Dial + Quantity;
      end if;
   end Turn_Dial;

begin
   Open (Document, In_File, Doc_name);
   while not End_Of_File (Document) loop
      declare
         Doc_Line : constant String := Get_Line (Document);
      begin
         Put_Line ("Read rotation " & Doc_Line);
         Turn_Dial (Doc_Line);
         Put_Line ("Current dial position: " & Dial_Position'Image (Dial));
         if Dial = 0 then
            Password := Password + 1;
         end if;
      end;
   end loop;
   Close (Document);
   Put_Line ("The password is " & Natural'Image (Password));
end Day1;
