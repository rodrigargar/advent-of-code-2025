with Ada.Text_IO; use Ada.Text_IO;

procedure Day4 is
   type Dimension is
      record
         X, Y : Positive;
      end record;

   function Get_Grid_Size (File_Name : String) return Dimension is
      Rolls_File : File_Type;
   begin
      Open (Rolls_File, In_File, File_Name);
      return Size : Dimension do
         declare
            Line : String := Get_Line (Rolls_File);
         begin
            Size.X := Line'Length;
            Size.Y := 1;
            while not End_Of_File (Rolls_File) loop
               Line := Get_Line (Rolls_File);
               Size.Y := Size.Y + 1;
            end loop;
         end;
         Close (Rolls_File);
      end return;
   end Get_Grid_Size;

   Grid_File_Name : constant String := "test.txt";
   Grid_Size : constant Dimension := Get_Grid_Size (Grid_File_Name);
   subtype Row is Positive range 1 .. Grid_Size.Y;
   subtype Column is Positive range 1 .. Grid_Size.X;

   Roll_Grid : array (Row'Range) of String (Column'Range);
   Roll_Here : array (Row'Range, Column'Range) of Boolean :=
      (others => (others => False));
   Adjacents : array (Row'Range, Column'Range) of Natural :=
      (others => (others => 0));
begin
   declare
      Grid_File : File_Type;
   begin
      Open (Grid_File, In_File, Grid_File_Name);
      for Grid_Line of Roll_Grid loop
         Grid_Line := Get_Line (Grid_File);
      end loop;
      Close (Grid_File);
   end;

   for M in Row'Range loop
      for N in Column'Range loop
         if Roll_Grid (M)(N) = '@' then
            Roll_Here (M, N) := True;
            if M + 1 in Row'Range then
               Adjacents (M + 1, N) := Adjacents (M + 1, N) + 1;
               if N + 1 in Column'Range then
                  Adjacents (M + 1, N + 1) := Adjacents (M + 1, N + 1) + 1;
               end if;
               if N - 1 in Column'Range then
                  Adjacents (M + 1, N - 1) := Adjacents (M + 1, N - 1) + 1;
               end if;
            end if;
            if M - 1 in Row'Range then
               Adjacents (M - 1, N) := Adjacents (M - 1, N) + 1;
               if N + 1 in Column'Range then
                  Adjacents (M - 1, N + 1) := Adjacents (M - 1, N + 1) + 1;
               end if;
               if N - 1 in Column'Range then
                  Adjacents (M - 1, N - 1) := Adjacents (M - 1, N - 1) + 1;
               end if;
            end if;
            if N + 1 in Column'Range then
               Adjacents (M, N + 1) := Adjacents (M, N + 1) + 1;
            end if;
            if N - 1 in Column'Range then
               Adjacents (M, N - 1) := Adjacents (M, N - 1) + 1;
            end if;
         end if;
      end loop;
   end loop;

   declare
      Accessible_Rolls : Natural := 0;
   begin
      for M in Row'Range loop
         for N in Column'Range loop
            if Roll_Here (M, N) and then Adjacents (M, N) < 4 then
               Accessible_Rolls := Accessible_Rolls + 1;
            end if;
         end loop;
      end loop;
      Put_Line ("Total accessible rolls:" & Accessible_Rolls'Image);
   end;
end Day4;
