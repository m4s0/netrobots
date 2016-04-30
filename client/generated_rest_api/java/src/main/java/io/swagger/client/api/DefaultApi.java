package io.swagger.client.api;

import com.sun.jersey.api.client.GenericType;

import io.swagger.client.ApiException;
import io.swagger.client.ApiClient;
import io.swagger.client.Configuration;
import io.swagger.client.Pair;

import io.swagger.client.model.RobotCommand;
import io.swagger.client.model.RobotStatus;
import io.swagger.client.model.RobotConfiguration;
import io.swagger.client.model.RemoveCommand;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavaClientCodegen", date = "2016-04-30T12:28:53.526+02:00")
public class DefaultApi {
  private ApiClient apiClient;

  public DefaultApi() {
    this(Configuration.getDefaultApiClient());
  }

  public DefaultApi(ApiClient apiClient) {
    this.apiClient = apiClient;
  }

  public ApiClient getApiClient() {
    return apiClient;
  }

  public void setApiClient(ApiClient apiClient) {
    this.apiClient = apiClient;
  }

  
  /**
   * 
   * send an action to a robot. The server will answer after the simulation turn is completed, and it will return the token for the next simulation turn.
   * @param command  (required)
   * @return RobotStatus
   * @throws ApiException if fails to make API call
   */
  public RobotStatus robotActionPost(RobotCommand command) throws ApiException {
    Object localVarPostBody = command;
    
    // verify the required parameter 'command' is set
    if (command == null) {
      throw new ApiException(400, "Missing the required parameter 'command' when calling robotActionPost");
    }
    
    // create path and map variables
    String localVarPath = "/robot-action".replaceAll("\\{format\\}","json");

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();

    

    

    

    final String[] localVarAccepts = {
      "application/json"
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      "application/json"
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };

    
    GenericType<RobotStatus> localVarReturnType = new GenericType<RobotStatus>() {};
    return apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    
  }
  
  /**
   * 
   * Create a new robot.
   * @param configuration  (required)
   * @return RobotStatus
   * @throws ApiException if fails to make API call
   */
  public RobotStatus robotCreatePost(RobotConfiguration configuration) throws ApiException {
    Object localVarPostBody = configuration;
    
    // verify the required parameter 'configuration' is set
    if (configuration == null) {
      throw new ApiException(400, "Missing the required parameter 'configuration' when calling robotCreatePost");
    }
    
    // create path and map variables
    String localVarPath = "/robot-create".replaceAll("\\{format\\}","json");

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();

    

    

    

    final String[] localVarAccepts = {
      "application/json"
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      "application/json"
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };

    
    GenericType<RobotStatus> localVarReturnType = new GenericType<RobotStatus>() {};
    return apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    
  }
  
  /**
   * 
   * Remove a robot.
   * @param remove  (required)
   * @throws ApiException if fails to make API call
   */
  public void robotRemovePost(RemoveCommand remove) throws ApiException {
    Object localVarPostBody = remove;
    
    // verify the required parameter 'remove' is set
    if (remove == null) {
      throw new ApiException(400, "Missing the required parameter 'remove' when calling robotRemovePost");
    }
    
    // create path and map variables
    String localVarPath = "/robot-remove".replaceAll("\\{format\\}","json");

    // query params
    List<Pair> localVarQueryParams = new ArrayList<Pair>();
    Map<String, String> localVarHeaderParams = new HashMap<String, String>();
    Map<String, Object> localVarFormParams = new HashMap<String, Object>();

    

    

    

    final String[] localVarAccepts = {
      "application/json"
    };
    final String localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);

    final String[] localVarContentTypes = {
      "application/json"
    };
    final String localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

    String[] localVarAuthNames = new String[] {  };

    
    apiClient.invokeAPI(localVarPath, "POST", localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, null);
    
  }
  
}
