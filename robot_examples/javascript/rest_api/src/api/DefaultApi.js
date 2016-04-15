(function(root, factory) {
  if (typeof define === 'function' && define.amd) {
    // AMD. Register as an anonymous module.
    define(['../ApiClient', '../model/BoardInfo', '../model/RobotCommand', '../model/RobotStatus', '../model/RobotConfiguration'], factory);
  } else if (typeof module === 'object' && module.exports) {
    // CommonJS-like environments that support module.exports, like Node.
    module.exports = factory(require('../ApiClient'), require('../model/BoardInfo'), require('../model/RobotCommand'), require('../model/RobotStatus'), require('../model/RobotConfiguration'));
  } else {
    // Browser globals (root is window)
    if (!root.NetRobotsApi) {
      root.NetRobotsApi = {};
    }
    root.NetRobotsApi.DefaultApi = factory(root.NetRobotsApi.ApiClient, root.NetRobotsApi.BoardInfo, root.NetRobotsApi.RobotCommand, root.NetRobotsApi.RobotStatus, root.NetRobotsApi.RobotConfiguration);
  }
}(this, function(ApiClient, BoardInfo, RobotCommand, RobotStatus, RobotConfiguration) {
  'use strict';

  /**
   * Default service.
   * @module api/DefaultApi
   * @version 2.0.0
   */

  /**
   * Constructs a new DefaultApi. 
   * @alias module:api/DefaultApi
   * @class
   * @param {module:ApiClient} apiClient Optional API client implementation to use, default to {@link module:ApiClient#instance}
   * if unspecified.
   */
  var exports = function(apiClient) {
    this.apiClient = apiClient || ApiClient.instance;


    /**
     * Callback function to receive the result of the boardEventsGet operation.
     * @callback module:api/DefaultApi~boardEventsGetCallback
     * @param {String} error Error message, if any.
     * @param {module:model/BoardInfo} data The data returned by the service call.
     * @param {String} response The complete HTTP response.
     */

    /**
     * Board Info
     * @param {module:api/DefaultApi~boardEventsGetCallback} callback The callback function, accepting three arguments: error, data, response
     * data is of type: {module:model/BoardInfo}
     */
    this.boardEventsGet = function(callback) {
      var postBody = null;


      var pathParams = {
      };
      var queryParams = {
      };
      var headerParams = {
      };
      var formParams = {
      };

      var authNames = [];
      var contentTypes = ['application/json'];
      var accepts = ['application/json'];
      var returnType = BoardInfo;

      return this.apiClient.callApi(
        '/board-events', 'GET',
        pathParams, queryParams, headerParams, formParams, postBody,
        authNames, contentTypes, accepts, returnType, callback
      );
    }

    /**
     * Callback function to receive the result of the boardInfoGet operation.
     * @callback module:api/DefaultApi~boardInfoGetCallback
     * @param {String} error Error message, if any.
     * @param {module:model/BoardInfo} data The data returned by the service call.
     * @param {String} response The complete HTTP response.
     */

    /**
     * Board Info
     * @param {module:api/DefaultApi~boardInfoGetCallback} callback The callback function, accepting three arguments: error, data, response
     * data is of type: {module:model/BoardInfo}
     */
    this.boardInfoGet = function(callback) {
      var postBody = null;


      var pathParams = {
      };
      var queryParams = {
      };
      var headerParams = {
      };
      var formParams = {
      };

      var authNames = [];
      var contentTypes = ['application/json'];
      var accepts = ['application/json'];
      var returnType = BoardInfo;

      return this.apiClient.callApi(
        '/board-info', 'GET',
        pathParams, queryParams, headerParams, formParams, postBody,
        authNames, contentTypes, accepts, returnType, callback
      );
    }

    /**
     * Callback function to receive the result of the robotActionPost operation.
     * @callback module:api/DefaultApi~robotActionPostCallback
     * @param {String} error Error message, if any.
     * @param {module:model/RobotStatus} data The data returned by the service call.
     * @param {String} response The complete HTTP response.
     */

    /**
     * Send an action to a robot. The server will answer after the simulation turn is completed, and it will return the token for the next simulation turn.
     * @param {module:model/RobotCommand} command 
     * @param {module:api/DefaultApi~robotActionPostCallback} callback The callback function, accepting three arguments: error, data, response
     * data is of type: {module:model/RobotStatus}
     */
    this.robotActionPost = function(command, callback) {
      var postBody = command;

      // verify the required parameter 'command' is set
      if (command == undefined || command == null) {
        throw "Missing the required parameter 'command' when calling robotActionPost";
      }


      var pathParams = {
      };
      var queryParams = {
      };
      var headerParams = {
      };
      var formParams = {
      };

      var authNames = [];
      var contentTypes = ['application/json'];
      var accepts = ['application/json'];
      var returnType = RobotStatus;

      return this.apiClient.callApi(
        '/robot-action', 'POST',
        pathParams, queryParams, headerParams, formParams, postBody,
        authNames, contentTypes, accepts, returnType, callback
      );
    }

    /**
     * Callback function to receive the result of the robotCreatePost operation.
     * @callback module:api/DefaultApi~robotCreatePostCallback
     * @param {String} error Error message, if any.
     * @param {module:model/RobotStatus} data The data returned by the service call.
     * @param {String} response The complete HTTP response.
     */

    /**
     * Create a new robot.
     * @param {module:model/RobotConfiguration} configuration 
     * @param {module:api/DefaultApi~robotCreatePostCallback} callback The callback function, accepting three arguments: error, data, response
     * data is of type: {module:model/RobotStatus}
     */
    this.robotCreatePost = function(configuration, callback) {
      var postBody = configuration;

      // verify the required parameter 'configuration' is set
      if (configuration == undefined || configuration == null) {
        throw "Missing the required parameter 'configuration' when calling robotCreatePost";
      }


      var pathParams = {
      };
      var queryParams = {
      };
      var headerParams = {
      };
      var formParams = {
      };

      var authNames = [];
      var contentTypes = ['application/json'];
      var accepts = ['application/json'];
      var returnType = RobotStatus;

      return this.apiClient.callApi(
        '/robot-create', 'POST',
        pathParams, queryParams, headerParams, formParams, postBody,
        authNames, contentTypes, accepts, returnType, callback
      );
    }
  };

  return exports;
}));
