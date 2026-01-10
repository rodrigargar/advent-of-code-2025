with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings;
with Ada.Strings.Maps;
with Ada.Strings.Fixed;
with Ada.Containers.Vectors;
with Ada.Containers.Generic_Array_Sort;
with Ada.Containers.Ordered_Sets;

procedure Day8 is
   Boxes_File_Name : constant String := "test.txt";
   Connections : constant Positive := 10;

   type Position is
      record
         X, Y, Z : Long_Integer := 0;
      end record;

   type Pair_Distance is
      record
         A, B : Positive;
         Distance : Long_Integer;
      end record;

   function "<" (Left, Right : Pair_Distance) return Boolean is
   begin
      return Left.Distance < Right.Distance;
   end "<";

   function Compute_Distance (A, B : Position) return Long_Integer is
   begin
      return (A.X - B.X) ** 2 + (A.Y - B.Y) ** 2 + (A.Z - B.Z) ** 2;
   end Compute_Distance;

   type Junction_Boxes is array (Positive range <>) of Position;
   type Box_Distances is array (Positive range <>) of Pair_Distance;

   function Get_Boxes (File_Name : String) return Junction_Boxes is
      package Str renames Ada.Strings;

      Boxes_File : File_Type;
      N_Boxes : Natural := 0;
      Comma : constant Str.Maps.Character_Set := Str.Maps.To_Set (',');
   begin
      Open (Boxes_File, In_File, File_Name);
      while not End_Of_File (Boxes_File) loop
         Skip_Line (Boxes_File);
         N_Boxes := N_Boxes + 1;
      end loop;
      Reset (Boxes_File);
      return Boxes : Junction_Boxes (1 .. N_Boxes) do
         for Box of Boxes loop
            declare
               Line : constant String := Get_Line (Boxes_File);
               First_Comma, Second_Comma : Natural;
            begin
               First_Comma := Str.Fixed.Index (Line, Comma);
               Second_Comma := Str.Fixed.Index (Line, Comma, First_Comma + 1);
               Box.X := Long_Integer'Value
                  (Line (Line'First .. First_Comma - 1));
               Box.Y := Long_Integer'Value
                  (Line (First_Comma + 1 .. Second_Comma - 1));
               Box.Z := Long_Integer'Value
                  (Line (Second_Comma + 1 .. Line'Last));
            end;
         end loop;
      end return;
   end Get_Boxes;

   Boxes : constant Junction_Boxes := Get_Boxes (Boxes_File_Name);

   function Get_Distances (B : Junction_Boxes) return Box_Distances is
      Index : Positive := 1;
   begin
      return D : Box_Distances (1 .. B'Last * (B'Last - 1) / 2) do
         for I in B'First .. B'Last - 1 loop
            for J in I + 1 .. B'Last loop
               D (Index) := Pair_Distance'(A => I, B => J,
                  Distance => Compute_Distance (B (I), B (J)));
               Index := Index + 1;
            end loop;
         end loop;
      end return;
   end Get_Distances;

   procedure Sort_Distance_Pairs is new
      Ada.Containers.Generic_Array_Sort (
         Index_Type => Positive,
         Element_Type => Pair_Distance,
         Array_Type => Box_Distances);

   Distances : Box_Distances := Get_Distances (Boxes);

   package Box_Sets is new
      Ada.Containers.Ordered_Sets (Element_Type => Positive);

   subtype Closest is Positive range 1 .. Connections;
   type Box_Groups is array (Positive range <>) of Box_Sets.Set;
   type Groups_Access is access Box_Groups;
   Circuits : constant Groups_Access := new Box_Groups (Distances'Range);

   function "<" (Left, Right : Box_Sets.Set) return Boolean is
      use Ada.Containers;
   begin
      return Left.Length > Right.Length;
   end "<";

   procedure Sort_Circuits is new
      Ada.Containers.Generic_Array_Sort (
         Index_Type => Positive,
         Element_Type => Box_Sets.Set,
         Array_Type => Box_Groups);
begin
   Sort_Distance_Pairs (Distances);
   for C in Closest'Range loop
      Circuits (C).Include (Distances (C).A);
      Circuits (C).Include (Distances (C).B);
   end loop;
   declare
      Found_Overlap : Boolean := False;
   begin
      loop
         Found_Overlap := False;
         for I in Closest'First + 1 .. Closest'Last loop
            for J in Closest'First .. I - 1 loop
               if Circuits (I).Overlap (Circuits (J)) then
                  Circuits (I).Union (Circuits (J));
                  Circuits (J).Clear;
                  Found_Overlap := True;
               end if;
            end loop;
         end loop;
         exit when not Found_Overlap;
      end loop;
   end;
   Sort_Circuits (Circuits.all);
   declare
      Product : constant Long_Integer :=
         [for C in 1 .. 3 =>
            Long_Integer (Circuits (C).Length)]'Reduce ("*", 1);
   begin
      Put_Line ("Product of the sizes of the biggest 3 circuits" &
         Product'Image);
   end;
end Day8;
