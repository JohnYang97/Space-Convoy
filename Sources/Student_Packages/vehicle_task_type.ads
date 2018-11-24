with Ada.Task_Identification; use Ada.Task_Identification;
with Ada.Containers.Hashed_Sets; use Ada.Containers;

-- Author : Wenjun Yang
-- u_id : u6251843

package Vehicle_Task_Type is

   task type Vehicle_Task is
      entry Identify (Set_Vehicle_No : Positive; Local_Task_Id : out Task_Id);
   end Vehicle_Task;

   -- Hash function used for the following HashSet and HashMap in veicle_task_type.adb file.
   function ID_Hashed (id : Positive) return Hash_Type is (Hash_Type (id));

   -- Define a HashSet to store the live and dead vehicles' number for every drone.
   package My_Set is new Ada.Containers.Hashed_Sets (Element_Type => Positive,
                                                     Hash => ID_Hashed,
                                                     Equivalent_Elements => "=");
   use My_Set;

   -- Discuss the use of subtype with Yiluo Wei (u6227375)
   -- Define a subtype of My_Set, the type of live and dead vehicles' number set (Two sets).
   subtype No_Set is My_Set.Set;

end Vehicle_Task_Type;
