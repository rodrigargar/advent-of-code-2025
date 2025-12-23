with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

procedure Day2 is
   function Read_Ranges (File_Name : String) return String is
      Id_File : File_Type;
   begin
      Open (Id_File, In_File, File_Name);
      return Ranges : constant String := Get_Line (Id_File) do
         Close (Id_File);
      end return;
   end Read_Ranges;

   function Is_Invalid (Id : String) return Boolean is
   begin
      -- To get the solution for part one of the puzzle,
      -- replace the line below by:
      -- for Reps in 2 .. 2 loop
      for Reps in 2 .. Id'Length loop
         if Id'Length mod Reps = 0 then
            if Id = Reps * Id (Id'First .. Id'Length / Reps) then
               return True;
            end if;
         end if;
      end loop;
      return False;
   end Is_Invalid;

   Id_Ranges : constant String := Read_Ranges ("test.txt");
   Comma : constant Character_Set := To_Set (',');
   Dash : constant Character_Set := To_Set ('-');
   Position, Id_First : Positive := Id_Ranges'First;
   Id_Last : Natural := Natural'First;
   Sum_Invalid_Ids : Long_Integer := 0;
begin
   while Position in Id_Ranges'Range loop
      Find_Token (Source => Id_Ranges,
                  Set => Comma,
                  From => Position,
                  Test => Outside,
                  First => Id_First,
                  Last => Id_Last);
      exit when Id_Last = 0;
      Put_Line ("Inspecting range: " & Id_Ranges (Id_First .. Id_Last));
      declare
         Id_Range : constant String := Id_Ranges (Id_First .. Id_Last);
         Dash_Position : constant Natural := Index (Id_Range, Dash);
         Lower_Id : constant String :=
            Id_Range (Id_Range'First .. Dash_Position - 1);
         Higher_Id : constant String :=
            Id_Range (Dash_Position + 1 .. Id_Range'Last);
      begin
         for Current_Id in Long_Integer'Value (Lower_Id) ..
                           Long_Integer'Value (Higher_Id) loop
            if Is_Invalid (Trim (Current_Id'Image, Ada.Strings.Left)) then
               Put_Line ("Found invalid ID:" & Current_Id'Image);
               Sum_Invalid_Ids := Sum_Invalid_Ids + Current_Id;
            end if;
         end loop;
      end;
      Position := Id_Last + 1;
   end loop;
   Put_Line ("The sum of all invalid IDs is:" & Sum_Invalid_Ids'Image);
end Day2;
