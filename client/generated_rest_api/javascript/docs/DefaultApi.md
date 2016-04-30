# NetRobotsApi.DefaultApi

All URIs are relative to *http://localhost*

Method | HTTP request | Description
------------- | ------------- | -------------
[**robotActionPost**](DefaultApi.md#robotActionPost) | **POST** /robot-action | 
[**robotCreatePost**](DefaultApi.md#robotCreatePost) | **POST** /robot-create | 
[**robotRemovePost**](DefaultApi.md#robotRemovePost) | **POST** /robot-remove | 


<a name="robotActionPost"></a>
# **robotActionPost**
> RobotStatus robotActionPost(command)



send an action to a robot. The server will answer after the simulation turn is completed, and it will return the token for the next simulation turn.

### Example
```javascript
var NetRobotsApi = require('net-robots-api');

var apiInstance = new NetRobotsApi.DefaultApi()

var command = new NetRobotsApi.RobotCommand(); // {RobotCommand} 


var callback = function(error, data, response) {
  if (error) {
    console.error(error);
  } else {
    console.log('API called successfully. Returned data: ' + data);
  }
};
api.robotActionPost(command, callback);
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

<a name="robotCreatePost"></a>
# **robotCreatePost**
> RobotStatus robotCreatePost(configuration)



Create a new robot.

### Example
```javascript
var NetRobotsApi = require('net-robots-api');

var apiInstance = new NetRobotsApi.DefaultApi()

var configuration = new NetRobotsApi.RobotConfiguration(); // {RobotConfiguration} 


var callback = function(error, data, response) {
  if (error) {
    console.error(error);
  } else {
    console.log('API called successfully. Returned data: ' + data);
  }
};
api.robotCreatePost(configuration, callback);
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

<a name="robotRemovePost"></a>
# **robotRemovePost**
> robotRemovePost(remove)



Remove a robot.

### Example
```javascript
var NetRobotsApi = require('net-robots-api');

var apiInstance = new NetRobotsApi.DefaultApi()

var remove = new NetRobotsApi.RemoveCommand(); // {RemoveCommand} 


var callback = function(error, data, response) {
  if (error) {
    console.error(error);
  } else {
    console.log('API called successfully.');
  }
};
api.robotRemovePost(remove, callback);
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **remove** | [**RemoveCommand**](RemoveCommand.md)|  | 

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP reuqest headers

 - **Content-Type**: application/json
 - **Accept**: application/json

