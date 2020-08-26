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

    A: Int_Array (1..15) := (9, 81, 92, 25, 38, 23, 4, 37, 34, 5, 45, 63, 57, 53, 11);
begin
    Do_Bubble_Sort(A);

    for I in A'Range loop
        Ada.Integer_Text_IO.Put(A(I), 0);
        Ada.Text_IO.New_Line;
    end loop;
end Bubble_Sort;
