-- Suggestions for packages which might be useful:

with Ada.Real_Time;         use Ada.Real_Time;
with Vectors_3D;            use Vectors_3D;
with Swarm_Structures_Base; use Swarm_Structures_Base;
with Vehicle_Task_Type; use Vehicle_Task_Type;

-- Author : Wenjun Yang
-- u_id : u6251843

package Vehicle_Message_Type is

   -- Replace this record definition by what your vehicles need to communicate.

   type Inter_Vehicle_Messages is

      record
         -- Vehicle_ID in the message, for recognizing the source of message
         ID                : Positive;
         -- The message sent time
         Message_Send_Time : Time;
         -- Record whether the drone finds the globe or not
         Energy_Globe_Find : Boolean;
         -- Record the position of energy globe
         Energy_Globe_Pos  : Vector_3D;
         -- Record the current energy level of corresponding drone
         My_Energy         : Vehicle_Charges;
         -- Message of all the live drones' vehicle_No
         Exist_Neighbours_No  : No_Set;
         -- Message of all the dead drones' vehicle_No
         Delete_Neighbours_No : No_Set;

      end record;

end Vehicle_Message_Type;
