with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Bubble_Sort is
    type Int_Array is array (Positive range <>) of Integer;

    procedure Do_Bubble_Sort(A: in out Int_Array) is
        Did_Swap: Boolean;
        Tmp: Positive;
    begin
        loop
            Did_Swap := false;
            for I in Integer range (A'First + 1) .. A'Last loop
                if A(I) < A(I - 1) then
                    Tmp := A(I - 1);
                    A(I - 1) := A(I);
                    A(I) := Tmp;
                    Did_Swap := true;
                end if;
            end loop;
            exit when not Did_Swap;
        end loop;
    end Do_Bubble_Sort;

    A: Int_Array (1..10) := (7, 6, 3, 5, 9, 4, 8, 10, 1, 2);
--    A: Int_Array (1..10) := (1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
--    A: Int_Array (1..10) := (10, 9, 8, 7, 6, 5, 4, 3, 2, 1);
--    A: Int_Array (1..1) := (others => 42);
--    A: Int_Array (1..0);
begin
    Do_Bubble_Sort(A);

    for I in A'Range loop
        Ada.Integer_Text_IO.Put(A(I), 0);
        if I /= A'Last then
            Ada.Text_IO.Put(", ");
        end if;
    end loop;
    Ada.Text_IO.New_Line;
end Bubble_Sort;
