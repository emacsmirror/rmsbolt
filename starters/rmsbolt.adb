with Ada.Text_IO; use Ada.Text_IO;

--  Ada rmsbolt starter file

--  Local Variables:
--  rmsbolt-command: "gcc -O0"
--  rmsbolt-disassemble: nil
--  End:

procedure RMSbolt is
   function IsRMS(A : Character) return Integer is
   begin
      case A is
         when 'R' => return 1;
         when 'M' => return 2;
         when 'S' => return 3;
         when others => return 0;
      end case;
   end IsRMS;

   A : Character := Character'Val (1 + 1);
begin
   if IsRMS(A) /= 0 then
      Put_Line (A'Image);
   end if;
end RMSbolt;
