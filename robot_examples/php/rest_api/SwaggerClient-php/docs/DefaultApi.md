# Swagger\Client\DefaultApi

All URIs are relative to *http://localhost*

Method | HTTP request | Description
------------- | ------------- | -------------
[**boardEventsGet**](DefaultApi.md#boardEventsGet) | **GET** /board-events | 
[**boardInfoGet**](DefaultApi.md#boardInfoGet) | **GET** /board-info | 
[**robotActionPost**](DefaultApi.md#robotActionPost) | **POST** /robot-action | 
[**robotCreatePost**](DefaultApi.md#robotCreatePost) | **POST** /robot-create | 


# **boardEventsGet**
> \Swagger\Client\Model\BoardInfo boardEventsGet()



Board Info

### Example 
```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');

$api_instance = new Swagger\Client\Api\DefaultApi();

try { 
    $result = $api_instance->boardEventsGet();
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling DefaultApi->boardEventsGet: ', $e->getMessage(), "\n";
}
?>
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**\Swagger\Client\Model\BoardInfo**](BoardInfo.md)

### Authorization

No authorization required

### HTTP reuqest headers

 - **Content-Type**: application/json
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **boardInfoGet**
> \Swagger\Client\Model\BoardInfo boardInfoGet()



Board Info

### Example 
```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');

$api_instance = new Swagger\Client\Api\DefaultApi();

try { 
    $result = $api_instance->boardInfoGet();
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling DefaultApi->boardInfoGet: ', $e->getMessage(), "\n";
}
?>
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**\Swagger\Client\Model\BoardInfo**](BoardInfo.md)

### Authorization

No authorization required

### HTTP reuqest headers

 - **Content-Type**: application/json
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **robotActionPost**
> \Swagger\Client\Model\RobotStatus robotActionPost($command)



Send an action to a robot. The server will answer after the simulation turn is completed, and it will return the token for the next simulation turn.

### Example 
```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');

$api_instance = new Swagger\Client\Api\DefaultApi();
$command = new \Swagger\Client\Model\RobotCommand(); // \Swagger\Client\Model\RobotCommand | 

try { 
    $result = $api_instance->robotActionPost($command);
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling DefaultApi->robotActionPost: ', $e->getMessage(), "\n";
}
?>
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **command** | [**\Swagger\Client\Model\RobotCommand**](\Swagger\Client\Model\RobotCommand.md)|  | 

### Return type

[**\Swagger\Client\Model\RobotStatus**](RobotStatus.md)

### Authorization

No authorization required

### HTTP reuqest headers

 - **Content-Type**: application/json
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **robotCreatePost**
> \Swagger\Client\Model\RobotStatus robotCreatePost($configuration)



Create a new robot.

### Example 
```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');

$api_instance = new Swagger\Client\Api\DefaultApi();
$configuration = new \Swagger\Client\Model\RobotConfiguration(); // \Swagger\Client\Model\RobotConfiguration | 

try { 
    $result = $api_instance->robotCreatePost($configuration);
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling DefaultApi->robotCreatePost: ', $e->getMessage(), "\n";
}
?>
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **configuration** | [**\Swagger\Client\Model\RobotConfiguration**](\Swagger\Client\Model\RobotConfiguration.md)|  | 

### Return type

[**\Swagger\Client\Model\RobotStatus**](RobotStatus.md)

### Authorization

No authorization required

### HTTP reuqest headers

 - **Content-Type**: application/json
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

