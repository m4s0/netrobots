# RobotCommand

## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**token** | **string** | A unique token, that changes after a command is sent from a robot to the server.\nIt is used for recognizing a robot, and chaining the commands, because a robot can not issue another command, before it receive an answer from the server, with the next token to use.\n | 
**fire** | [**\Swagger\Client\Model\FireCommand**](FireCommand.md) |  | [optional] 
**drive** | [**\Swagger\Client\Model\DriveCommand**](DriveCommand.md) |  | [optional] 
**scan** | [**\Swagger\Client\Model\ScanCommand**](ScanCommand.md) |  | [optional] 

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


