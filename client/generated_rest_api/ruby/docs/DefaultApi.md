# SwaggerClient::DefaultApi

All URIs are relative to *http://localhost*

Method | HTTP request | Description
------------- | ------------- | -------------
[**robot_action_post**](DefaultApi.md#robot_action_post) | **POST** /robot-action | 
[**robot_create_post**](DefaultApi.md#robot_create_post) | **POST** /robot-create | 
[**robot_remove_post**](DefaultApi.md#robot_remove_post) | **POST** /robot-remove | 


# **robot_action_post**
> RobotStatus robot_action_post(command)



send an action to a robot. The server will answer after the simulation turn is completed, and it will return the token for the next simulation turn.

### Example
```ruby
# load the gem
require 'swagger_client'

api_instance = SwaggerClient::DefaultApi.new

command = SwaggerClient::RobotCommand.new # RobotCommand | 


begin
  result = api_instance.robot_action_post(command)
  p result
rescue SwaggerClient::ApiError => e
  puts "Exception when calling DefaultApi->robot_action_post: #{e}"
end
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **command** | [**RobotCommand**](RobotCommand.md)|  | 

### Return type

[**RobotStatus**](RobotStatus.md)

### Authorization

No authorization required

### HTTP reuqest headers

 - **Content-Type**: application/json
 - **Accept**: application/json



# **robot_create_post**
> RobotStatus robot_create_post(configuration)



Create a new robot.

### Example
```ruby
# load the gem
require 'swagger_client'

api_instance = SwaggerClient::DefaultApi.new

configuration = SwaggerClient::RobotConfiguration.new # RobotConfiguration | 


begin
  result = api_instance.robot_create_post(configuration)
  p result
rescue SwaggerClient::ApiError => e
  puts "Exception when calling DefaultApi->robot_create_post: #{e}"
end
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **configuration** | [**RobotConfiguration**](RobotConfiguration.md)|  | 

### Return type

[**RobotStatus**](RobotStatus.md)

### Authorization

No authorization required

### HTTP reuqest headers

 - **Content-Type**: application/json
 - **Accept**: application/json



# **robot_remove_post**
> robot_remove_post(remove)



Remove a robot.

### Example
```ruby
# load the gem
require 'swagger_client'

api_instance = SwaggerClient::DefaultApi.new

remove = SwaggerClient::RemoveCommand.new # RemoveCommand | 


begin
  api_instance.robot_remove_post(remove)
rescue SwaggerClient::ApiError => e
  puts "Exception when calling DefaultApi->robot_remove_post: #{e}"
end
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **remove** | [**RemoveCommand**](RemoveCommand.md)|  | 

### Return type

nil (empty response body)

### Authorization

No authorization required

### HTTP reuqest headers

 - **Content-Type**: application/json
 - **Accept**: application/json



