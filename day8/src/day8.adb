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
      --  No need to take the square root for comparing distances
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

   type Box_Set_Access is access Box_Sets.Set;

   subtype Closest is Positive range 1 .. Connections;

   package Circuits_Vectors is new Ada.Containers.Vectors (
      Index_Type => Positive,
      Element_Type => Box_Set_Access);

   function "<" (Left, Right : Box_Set_Access) return Boolean is
      use Ada.Containers;
   begin
      return Left.Length > Right.Length;
   end "<";

   package Circuits_Sorting is new Circuits_Vectors.Generic_Sorting;

   Circuits : Circuits_Vectors.Vector;

   procedure Reduce_Circuits is
   begin
      for I in Circuits.First_Index .. Circuits.Last_Index - 1 loop
         for J in I + 1 .. Circuits.Last_Index loop
            if Circuits (I).all.Overlap (Circuits (J).all) then
               Circuits (I).all.Union (Circuits (J).all);
               Circuits (J).all.Clear;
               Circuits.Delete (J);
               return;
            end if;
         end loop;
      end loop;
   end Reduce_Circuits;

   procedure Insert_Pair (D : Pair_Distance; Found : out Boolean) is
   begin
      Found := False;
      for S of Circuits loop
         if S.Contains (D.A) then
            S.Include (D.B);
            Found := True;
            exit;
         elsif S.Contains (D.B) then
            S.Include (D.A);
            Found := True;
            exit;
         end if;
      end loop;
      if not Found then
         Circuits.Append (new Box_Sets.Set);
         Circuits.Last_Element.Insert (D.A);
         Circuits.Last_Element.Insert (D.B);
      end if;
   end Insert_Pair;
begin
   Sort_Distance_Pairs (Distances);

   --  First part
   for C in Closest'Range loop
      declare
         Junction_Found : Boolean := False;
      begin
         Insert_Pair (Distances (C), Junction_Found);
         if Junction_Found then
            Reduce_Circuits;
         end if;
      end;
   end loop;
   Circuits_Sorting.Sort (Circuits);
   declare
      Product : constant Long_Integer :=
         [for C in 1 .. 3 =>
            Long_Integer (Circuits (C).all.Length)]'Reduce ("*", 1);
   begin
      Put_Line ("Product of the sizes of the biggest 3 circuits" &
         Product'Image);
   end;

   --  Second part
   for C in Closest'Last + 1 .. Distances'Last loop
      declare
         use Ada.Containers;
         Junction_Found : Boolean := False;
      begin
         Insert_Pair (Distances (C), Junction_Found);
         if Junction_Found then
            Reduce_Circuits;
            if Circuits.Length = 1 and then
               Circuits (1).all.Length = Boxes'Length
            then
               Put_Line ("Product of the X coordinates " &
                  "of the last two junction boxes:" & Long_Integer'Image
                  (Boxes (Distances (C).A).X * Boxes (Distances (C).B).X));
               exit;
            end if;
         end if;
      end;
   end loop;
end Day8;
