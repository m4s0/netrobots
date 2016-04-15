# SwaggerClient::BoardInfo

## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**max_board_x** | **Float** |  | [optional] 
**max_board_y** | **Float** |  | [optional] 
**stream_delay** | **Float** | the time in real seconds, beetwen event lists | [optional] 
**turn_delta_time** | **Float** | Every robot can send and execute a command only after this simulated interval of time. | [optional] 
**network_latency** | **Float** | Every robot can send a command only after this interval of time. | [optional] 
**start_time** | **Float** | The events starts at the specified simulation time. | [optional] 
**end_time** | **Float** | The events ends at the specified simulation time. | [optional] 
**events** | [**Array&lt;Event&gt;**](Event.md) |  | [optional] 


