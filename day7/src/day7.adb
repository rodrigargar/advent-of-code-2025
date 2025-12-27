with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings;
with Ada.Strings.Maps;
with Ada.Strings.Fixed;

procedure Day7 is
   Diagram_File_Name : constant String := "test.txt";

   package Str renames Ada.Strings;

   type State is (Empty, Start, Beam, Splitter);
   State_To_Char : constant array (State'Range) of Character :=
      [Empty => '.', Start => 'S', Beam => '|', Splitter => '^'];
   State_Char_Pos_Mapping : constant Str.Maps.Character_Mapping :=
      Str.Maps.To_Mapping (".S|^", "1234");
   Pos_To_State : constant array (Character range '1' .. '4') of State :=
      ['1' => Empty, '2' => Start, '3' => Beam, '4' => Splitter];
   type Manifold is array (Positive range <>, Positive range <>) of State;

   function Get_Diagram (File_Name : String) return Manifold is
      Diagram_File : File_Type;
      X, Y : Natural := 0;
   begin
      Open (Diagram_File, In_File, File_Name);
      declare
         Line : constant String := Get_Line (Diagram_File);
      begin
         X := Line'Length;
         Y := 1;
         while not End_Of_File (Diagram_File) loop
            Skip_Line (Diagram_File);
            Y := Y + 1;
         end loop;
      end;
      Reset (Diagram_File);
      return D : Manifold (1 .. Y, 1 .. X) do
         for Row in D'Range (1) loop
            declare
               Line : constant String := Get_Line (Diagram_File);
               Mapped : constant String := Str.Fixed.Translate
                  (Line, State_Char_Pos_Mapping);
            begin
               for Column in Mapped'Range loop
                  D (Row, Column) := Pos_To_State (Mapped (Column));
               end loop;
            end;
         end loop;
         Close (Diagram_File);
      end return;
   end Get_Diagram;

   procedure Display_Manifold (D : Manifold) is
   begin
      for Row in D'Range (1) loop
         for Column in D'Range (2) loop
            Put (State_To_Char (D (Row, Column)));
         end loop;
         New_Line;
      end loop;
   end Display_Manifold;

   procedure Propagate_Beams (D : in out Manifold) is
      Splits : Natural := 0;
   begin
      --  First two lines
      for Column in D'Range (2) loop
         if D (1, Column) = Start then
            D (2, Column) := Beam;
         end if;
      end loop;
      --  Rest of manifold
      for Row in 3 .. D'Last (1) loop
         for Column in D'Range (2) loop
            if D (Row, Column) = Splitter then
               if D (Row - 1, Column) = Beam then
                  D (Row, Column - 1) := Beam;
                  D (Row, Column + 1) := Beam;
                  Splits := Splits + 1;
               end if;
            elsif D (Row, Column) = Empty then
               if D (Row - 1, Column) = Beam then
                  D (Row, Column) := Beam;
               end if;
            end if;
         end loop;
      end loop;
      Put_Line ("Total Splits:" & Splits'Image);
   end Propagate_Beams;

   function Quantum_Timelines (D : Manifold) return Long_Integer is
      Q : array (D'Range (1), D'Range (2)) of Long_Integer :=
         [others => [others => 0]];
   begin
      for Column in D'Range (2) loop
         if D (1, Column) = Start then
            Q (2, Column) := 1;
         end if;
      end loop;
      for Row in 3 .. D'Last (1) loop
         for Column in D'Range (2) loop
            if D (Row, Column) = Splitter then
               Q (Row, Column - 1) := Q (Row, Column - 1) +
                                      Q (Row - 1, Column);
               Q (Row, Column + 1) := Q (Row, Column + 1) +
                                      Q (Row - 1, Column);
            else
               Q (Row, Column) := Q (Row, Column) + Q (Row - 1, Column);
            end if;
         end loop;
      end loop;
      return [for C in Q'Range (2) => Q (Q'Last (1), C)]'Reduce ("+", 0);
   end Quantum_Timelines;

   Diagram : Manifold := Get_Diagram (Diagram_File_Name);
begin
   Propagate_Beams (Diagram);
   Display_Manifold (Diagram);
   Put_Line ("Timelines:" & Quantum_Timelines (Diagram)'Image);
end Day7;
