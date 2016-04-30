# SwaggerClient::RobotStatus

## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**name** | **String** |  | 
**token** | **String** | A unique token, that changes after a command is sent from a robot to the server.\nIt is used for recognizing a robot, and chaining the commands, because a robot can not issue another command, before it receive an answer from the server, with the next token to use.\n | 
**configuration** | [**RobotConfiguration**](RobotConfiguration.md) |  | 
**simulation_time** | **Float** | The current simulation time. | 
**time_tick** | **Float** | The next command will be executed at simulationTime + this value. Usually it is a constant value for all the course of the simulation. | 
**real_time_tick** | **Float** | The time in seconds, the system waits before processing the next request from remote robots.\nWith slow nework connections this value should be higher, because otherwise some remote robots could miss some game turns.\nNOTE: this time differs from timeIncrement, because timeIncrement is the simulation time that pass between two robots commands.\n | 
**health** | **Float** | The health of a robot. 0 when a robot is dead (completely destroyed). | 
**is_dead** | **BOOLEAN** |  | 
**is_winner** | **BOOLEAN** |  | 
**is_well_specified_robot** | **BOOLEAN** | true if the robot creation params respect the constraints. | 
**direction** | **Float** | Direction expressed in degrees. 0 degree is EAST, 90 degree is NORTH, 180 degree is WEST, 279 degree is SOUTH | 
**speed** | **Float** |  | 
**pos_x** | **Float** |  | 
**pos_y** | **Float** |  | 
**cannon_reloading_time** | **Float** | 0 if the robot can fire immediately, the remaining time it must wait otherwise. | 
**fired_new_missile** | **BOOLEAN** | True if the robot in last command fired a missile. | 
**scan_status** | [**ScanStatus**](ScanStatus.md) |  | [optional] 


