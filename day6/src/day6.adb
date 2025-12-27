with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings;
with Ada.Strings.Maps;
with Ada.Strings.Fixed;

procedure Day6 is
   package Str renames Ada.Strings;

   Problem_File_Name : constant String := "test.txt";

   type Problem_Size is
      record
         N_Operands, N_Operations : Natural := 0;
      end record;
   Operation_Set : constant Str.Maps.Character_Set := Str.Maps.To_Set ("+*");
   Space_Set : constant Str.Maps.Character_Set := Str.Maps.To_Set (" ");

   function Get_Problem_Size (File_Name : String) return Problem_Size is
      Problem_File : File_Type;
   begin
      Open (Problem_File, In_File, File_Name);
      return P : Problem_Size do
         while not End_Of_File (Problem_File) loop
            declare
               Line : constant String := Get_Line (Problem_File);
            begin
               if Str.Maps.Is_In (Line (1), Operation_Set) then
                  P.N_Operations := Str.Fixed.Count (Line, Operation_Set);
               else
                  P.N_Operands := P.N_Operands + 1;
               end if;
            end;
         end loop;
         Close (Problem_File);
      end return;
   end Get_Problem_Size;

   Full_Size : constant Problem_Size := Get_Problem_Size (Problem_File_Name);
   subtype Operand_Index is Positive range 1 .. Full_Size.N_Operands;
   type Operands_Type is array (Operand_Index'Range) of Long_Integer;

   type Subproblem_Type is
      record
         Operands : Operands_Type;
         Operation : Character;
      end record;

   subtype Subproblem_Index is Positive range 1 .. Full_Size.N_Operations;
   type Subproblems_Type is array (Subproblem_Index'Range) of Subproblem_Type;

   function Parse_Problem (File_Name : String) return Subproblems_Type is
      Problem_File : File_Type;
   begin
      Open (Problem_File, In_File, File_Name);
      return Problem : Subproblems_Type do
         for Count in Operand_Index'Range loop
            declare
               Operands_Line : constant String := Get_Line (Problem_File);
               Position : Positive := Operands_Line'First;
               Op_First : Positive;
               Op_Last : Natural;
            begin
               for Subproblem of Problem loop
                  Str.Fixed.Find_Token (Source => Operands_Line,
                                        Set => Space_Set,
                                        From => Position,
                                        Test => Str.Outside,
                                        First => Op_First,
                                        Last => Op_Last);
                  Subproblem.Operands (Count) :=
                     Long_Integer'Value (Operands_Line (Op_First .. Op_Last));
                  Position := Op_Last + 1;
               end loop;
            end;
         end loop;
         declare
            Operations_Line : constant String := Get_Line (Problem_File);
            Position : Positive := Operations_Line'First;
            Op_First : Positive;
            Op_Last : Natural;
         begin
            for Subproblem of Problem loop
               Str.Fixed.Find_Token (Source => Operations_Line,
                                     Set => Operation_Set,
                                     From => Position,
                                     Test => Str.Inside,
                                     First => Op_First,
                                     Last => Op_Last);
               Subproblem.Operation := Operations_Line (Op_First);
               Position := Op_Last + 1;
            end loop;
         end;
         Close (Problem_File);
      end return;
   end Parse_Problem;

   Problem : constant Subproblems_Type := Parse_Problem (Problem_File_Name);
   Solutions : array (Subproblem_Index'Range) of Long_Integer;

   --  Part 2
   type Dimension is
      record
         X, Y : Natural := 0;
      end record;

   function Get_Matrix_Size (File_Name : String) return Dimension is
      Problem_File : File_Type;
   begin
      Open (Problem_File, In_File, File_Name);
      declare
         Line : constant String := Get_Line (Problem_File);
      begin
         return D : Dimension do
            D.X := Line'Length;
            while not End_Of_File (Problem_File) loop
               Skip_Line (Problem_File);
               D.Y := D.Y + 1; --  ignore last line with operators +, *
            end loop;
            Close (Problem_File);
         end return;
      end;
   end Get_Matrix_Size;

   Matrix_Size : constant Dimension := Get_Matrix_Size (Problem_File_Name);
   type Char_Matrix is array (1 .. Matrix_Size.X) of
      String (1 .. Matrix_Size.Y);

   function Get_Char_Matrix (File_Name : String) return Char_Matrix is
      Problem_File : File_Type;
   begin
      Open (Problem_File, In_File, File_Name);
      return Matrix : Char_Matrix do
         for J in 1 .. Matrix_Size.Y loop
            declare
               Line : constant String := Get_Line (Problem_File);
            begin
               for I in 1 .. Matrix_Size.X loop
                  Matrix (I)(J) := Line (I); --  Transpose
               end loop;
            end;
         end loop;
         Close (Problem_File);
      end return;
   end Get_Char_Matrix;

   Matrix : constant Char_Matrix := Get_Char_Matrix (Problem_File_Name);
   Solutions_2 : array (Subproblem_Index'Range) of Long_Integer;

   procedure Init_Solutions_2 is
   begin
      for I in Subproblem_Index'Range loop
         if Problem (I).Operation = '+' then
            Solutions_2 (I) := 0;
         elsif Problem (I).Operation = '*' then
            Solutions_2 (I) := 1;
         end if;
      end loop;
   end Init_Solutions_2;

   procedure Compute_Solutions_2 is
      I : Subproblem_Index := Subproblem_Index'First;
   begin
      for Row of Matrix loop
         declare
            T_Row : constant String := Str.Fixed.Trim (Row, Str.Both);
         begin
            if T_Row'Length = 0 then
               I := I + 1;
            else
               if Problem (I).Operation = '+' then
                  Solutions_2 (I) :=
                     Solutions_2 (I) + Long_Integer'Value (T_Row);
               elsif Problem (I).Operation = '*' then
                  Solutions_2 (I) :=
                     Solutions_2 (I) * Long_Integer'Value (T_Row);
               end if;
            end if;
         end;
      end loop;
   end Compute_Solutions_2;

begin
   for I in Subproblem_Index'Range loop
      Put_Line ("Compute " & Problem (I).Operation);
      for Operand of Problem (I).Operands loop
         Put_Line (Operand'Image);
      end loop;
      if Problem (I).Operation = '+' then
         Solutions (I) := Problem (I).Operands'Reduce ("+", 0);
      elsif Problem (I).Operation = '*' then
         Solutions (I) := Problem (I).Operands'Reduce ("*", 1);
      end if;
      Put_Line ("Solution (" & I'Image & "):" & Solutions (I)'Image);
   end loop;
   Put_Line ("Total sum:" & Solutions'Reduce ("+", 0)'Image);

   --  Part 2
   Init_Solutions_2;
   Compute_Solutions_2;
   Put_Line ("Total sum part 2: " & Solutions_2'Reduce ("+", 0)'Image);
end Day6;
