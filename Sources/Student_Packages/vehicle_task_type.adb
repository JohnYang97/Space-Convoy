-- Suggestions for packages which might be useful:

with Ada.Real_Time;              use Ada.Real_Time;
--  with Ada.Text_IO;                use Ada.Text_IO;
with Exceptions;                 use Exceptions;
--  with Real_Type;                  use Real_Type;
--  with Generic_Sliding_Statistics;
--  with Rotations;                  use Rotations;
with Vectors_3D;                 use Vectors_3D;
with Vehicle_Interface;          use Vehicle_Interface;
with Vehicle_Message_Type; use Vehicle_Message_Type;
with Ada.Containers.Hashed_Maps;
with Swarm_Structures_Base; use Swarm_Structures_Base;
--  with Ada.Text_IO; use Ada.Text_IO;

-- Author : Wenjun Yang
-- u_id : u6251843

-- Discussing with Chenhao Tong, a student who is interested in distributed system design,
-- the overall design of system and basic algorithms used for coordination.
-- Discussing with Zhaoyu Feng (u6392260) and Yiluo Wei (u6227375) about the design and implementation of stage d.

package body Vehicle_Task_Type is

   task body Vehicle_Task is

      -- Vehicle_No of corresponding drone
      Vehicle_No : Positive;
      -- Decide whether the drone receives message before, true if the drone hasn't received any messages.
      Empty_Message_Box : Boolean := True;
      -- The message that will be sent by the drone
      Message_Sent : Inter_Vehicle_Messages;
      -- The message received by the drone (not useful if it is outdated)
      Message_Received : Inter_Vehicle_Messages;
      -- The message accepted by the drone (lastest message, contains the correct position of globe)
      Message_Accepted : Inter_Vehicle_Messages;
      -- Decide whether the drone needs charge. if so, then the drone goes to comparison.
      First_Line : Boolean := False;
      -- Decide whether the drone in a emergency mode. If so, the drone head to globe directly.
      Second_Line : Boolean := False;
      -- If the drone finds the globe, the parameter is set to true.
      Find_Energy : Boolean := False;
      -- Record the time when the drone finds the globe
      Find_Energy_Time : Time;
      -- Record the position of energy globe
      Global_Pos : Vector_3D;

      -- Record the vehicle_No and current energy level of other drones.
      package My_Hash is new Ada.Containers.Hashed_Maps (Key_Type => Positive,
                                                         Element_Type => Vehicle_Charges,
                                                         Hash => ID_Hashed,
                                                         Equivalent_Keys => "=");
      use My_Hash;

      Charge_Map : My_Hash.Map;

      -- Record the vehicle_No of live and dead vehicles.
      Vehicle_No_Set,
      Delete_No_Set : No_Set;

      -- Return the minimum energy level of all the drones
      function Minimum_Energy_Around return Vehicle_Charges is
         min : Vehicle_Charges := Full_Charge;
      begin
         for pair in Charge_Map.Iterate loop
            if Element (pair) <= min then
               min :=  Element (pair);
            end if;
         end loop;
         return min;
      end Minimum_Energy_Around;

      -- Return the Vehicle_No order of a specific drone in the live drone set
      function Position_In_Set (No : Positive) return Integer is
         Counter : Integer := 0;
      begin
         for value in Vehicle_No_Set.Iterate loop
            if Element (value) < No then
               Counter := Counter + 1;
            end if;
         end loop;
         Counter := Counter + 1;
         return Counter;
      end Position_In_Set;

   begin

      -- You need to react to this call and provide your task_id.
      -- You can e.g. employ the assigned vehicle number (Vehicle_No)
      -- in communications with other vehicles.

      accept Identify (Set_Vehicle_No : Positive; Local_Task_Id : out Task_Id) do
         Vehicle_No      := Set_Vehicle_No;
         Local_Task_Id   := Current_Task;
      end Identify;

      -- Replace the rest of this task with your own code.
      -- Maybe synchronizing on an external event clock like "Wait_For_Next_Physics_Update",
      -- yet you can synchronize on e.g. the real-time clock as well.

      -- Without control this vehicle will go for its natural swarming instinct.

      select

         Flight_Termination.Stop;

      then abort

         Outer_task_loop : loop

            declare
               Globes : constant Energy_Globes := Energy_Globes_Around;

            begin

               Wait_For_Next_Physics_Update;

               -- Define at what level of current charge, the drone start to charge and what level of current
               -- charge, the drone is in emergency mode.
               if Current_Charge <= 0.5 then
                  Second_Line := True;
               elsif Current_Charge <= 0.85 then
                  First_Line := True;
               end if;

               -- Record the vehicle_No and current charge level of this drone
               if Charge_Map.Contains (Vehicle_No) then
                  Charge_Map.Replace (Vehicle_No, Current_Charge);
               else
                  Charge_Map.Insert (Vehicle_No, Current_Charge);
               end if;

               -- Record the vehicle_No of this drone, which represents the drone is still alive
               if not Vehicle_No_Set.Contains (Vehicle_No) then
                  Vehicle_No_Set.Insert (Vehicle_No);
               end if;

               -- Message received part
               while Messages_Waiting loop
                  -- Receive the message
                  Receive (Message_Received);
                  -- Record the id and current energy level of other drones
                  if Charge_Map.Contains (Message_Received.ID) then
                     Charge_Map.Replace (Message_Received.ID, Message_Received.My_Energy);
                  else
                     Charge_Map.Insert (Message_Received.ID, Message_Received.My_Energy);
                  end if;

                  -- Update the message of live and dead drones' information by combining the outside info
                  -- with my own info.
                  Vehicle_No_Set.Union (Message_Received.Exist_Neighbours_No);
                  Delete_No_Set.Union (Message_Received.Delete_Neighbours_No);

                  -- if the drone has not received any messgae, receive any message even the message is outdated.
                  if Empty_Message_Box then
                     -- Have received a message, the message box is not empty any more.
                     Empty_Message_Box := False;
                     Message_Accepted := Message_Received;
                     -- Obtain the position of globe
                     Global_Pos := Message_Accepted.Energy_Globe_Pos;
                  elsif not Empty_Message_Box then
                     -- if the drone has received a message, compare the time of new message and received message,
                     -- accept the new message if it is the latest one.
                     if Message_Accepted.Message_Send_Time <= Message_Received.Message_Send_Time then
                        Message_Accepted := Message_Received;
                        Global_Pos := Message_Accepted.Energy_Globe_Pos;
                     end if;
                  end if;
               end loop;

               -- Update the live drones set by deleting the dead drones' number.
               Vehicle_No_Set.Difference (Delete_No_Set);

               -- Find energy
               -- Delete the position of globe if the info is received 3 seconds ago,
               -- the action will guarantee that the drone will always try to find a
               -- globe rather than depend on other drones.
               if Find_Energy and then (Clock - Find_Energy_Time) >= Seconds (3) then
                  Find_Energy := False;
               end if;

               -- Detect globes
               if Globes'Length > 0 then
                  if Globes'Length = 1 then
                     -- If there is a globe around, record the position and time when drone finds the globe.
                     Global_Pos := Globes (1).Position;
                     Find_Energy_Time := Clock;
                  else
                     -- If there is more than one globe around, always choose the closest globe.
                     Global_Pos := Globes (1).Position;
                     for Globe of Globes loop
                        if abs (Globe.Position - Position) < abs (Global_Pos - Position) then
                           Global_Pos := Globe.Position;
                        end if;
                     end loop;
                     Find_Energy_Time := Clock;
                  end if;

                  Find_Energy := True;

               end if;

               -- Message sent
               -- send the message
               Message_Sent := (ID                   => Vehicle_No,
                                Energy_Globe_Pos     => Global_Pos,
                                Message_Send_Time    => Clock,
                                Energy_Globe_Find    => Find_Energy,
                                My_Energy            => Current_Charge,
                                Exist_Neighbours_No  => Vehicle_No_Set,
                                Delete_Neighbours_No => Delete_No_Set
                               );
               Send (Message_Sent);

               -- Stage D implementation, delete the vehicle_No of entra drones if the current vehicle
               -- number is greater than Target_No_of_Elements
               if Position_In_Set (Vehicle_No) > Target_No_of_Elements then
                  Delete_No_Set.Insert (Vehicle_No);
                  -- Go to die
                  exit Outer_task_loop;
               end if;

               -- if the drone is in emergency mode, it heads to the globe immediately.
               if Second_Line then
                  -- Heading to destination
                  Set_Destination (Global_Pos);
                  -- In the largest speed
                  Set_Throttle (1.0);
               elsif (not Second_Line and then First_Line and then Find_Energy)
                 or else (not Second_Line and then First_Line and then (not Find_Energy) and then Message_Accepted.Energy_Globe_Find)
                   -- if the drone needs charge but not in emergency and it finds the position of globe OR
                   -- it doesn't find the position but receives the position from other drones, it should prepare to attend comparison.
               then
                  -- If its current charge is the lowest one among all the drones, the drone heads to globe.
                  if Current_Charge <= Minimum_Energy_Around then
                     Set_Destination (Global_Pos);
                     Set_Throttle (0.8);
                  else
                     -- otherwise, waiting for next round comparison
                     Set_Destination (Global_Pos);
                     Set_Throttle (0.2);
                  end if;
               end if;

               -- Update the information after the drone gets charged from the globe
               if Current_Charge = 1.0 then
                  First_Line                            := False;
                  Second_Line                           := False;
                  Set_Throttle (0.2);
                  -- Sending messages to other drones when the drone finshed charging
                  Message_Sent := (ID                   => Vehicle_No,
                                   Energy_Globe_Pos     => Global_Pos,
                                   Message_Send_Time    => Clock,
                                   Energy_Globe_Find    => Find_Energy,
                                   My_Energy            => Current_Charge,
                                   Exist_Neighbours_No  => Vehicle_No_Set,
                                   Delete_Neighbours_No => Delete_No_Set
                                  );
                  Send (Message_Sent);
               end if;

               -- Clear the drone's charge list to get everything updated.
               Charge_Map.Clear;

            end;

         end loop Outer_task_loop;

      end select;

   exception
      when E : others => Show_Exception (E);

   end Vehicle_Task;

end Vehicle_Task_Type;
