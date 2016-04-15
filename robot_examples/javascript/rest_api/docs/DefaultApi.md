# NetRobotsApi.DefaultApi

All URIs are relative to *http://localhost*

Method | HTTP request | Description
------------- | ------------- | -------------
[**boardEventsGet**](DefaultApi.md#boardEventsGet) | **GET** /board-events | 
[**boardInfoGet**](DefaultApi.md#boardInfoGet) | **GET** /board-info | 
[**robotActionPost**](DefaultApi.md#robotActionPost) | **POST** /robot-action | 
[**robotCreatePost**](DefaultApi.md#robotCreatePost) | **POST** /robot-create | 


<a name="boardEventsGet"></a>
# **boardEventsGet**
> BoardInfo boardEventsGet



Board Info

### Example
```javascript
var NetRobotsApi = require('net-robots-api');

var apiInstance = new NetRobotsApi.DefaultApi()

var callback = function(error, data, response) {
  if (error) {
    console.error(error);
  } else {
    console.log('API called successfully. Returned data: ' + data);
  }
};
api.boardEventsGet(callback);
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**BoardInfo**](BoardInfo.md)

### Authorization

No authorization required

### HTTP reuqest headers

 - **Content-Type**: application/json
 - **Accept**: application/json

<a name="boardInfoGet"></a>
# **boardInfoGet**
> BoardInfo boardInfoGet



Board Info

### Example
```javascript
var NetRobotsApi = require('net-robots-api');

var apiInstance = new NetRobotsApi.DefaultApi()

var callback = function(error, data, response) {
  if (error) {
    console.error(error);
  } else {
    console.log('API called successfully. Returned data: ' + data);
  }
};
api.boardInfoGet(callback);
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**BoardInfo**](BoardInfo.md)

### Authorization

No authorization required

### HTTP reuqest headers

 - **Content-Type**: application/json
 - **Accept**: application/json

<a name="robotActionPost"></a>
# **robotActionPost**
> RobotStatus robotActionPost(command)



Send an action to a robot. The server will answer after the simulation turn is completed, and it will return the token for the next simulation turn.

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

