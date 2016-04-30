(function(root, factory) {
  if (typeof define === 'function' && define.amd) {
    // AMD. Register as an anonymous module.
    define(['../ApiClient'], factory);
  } else if (typeof module === 'object' && module.exports) {
    // CommonJS-like environments that support module.exports, like Node.
    module.exports = factory(require('../ApiClient'));
  } else {
    // Browser globals (root is window)
    if (!root.NetRobotsApi) {
      root.NetRobotsApi = {};
    }
    root.NetRobotsApi.RemoveCommand = factory(root.NetRobotsApi.ApiClient);
  }
}(this, function(ApiClient) {
  'use strict';

  /**
   * The RemoveCommand model module.
   * @module model/RemoveCommand
   * @version 2.0
   */

  /**
   * Constructs a new <code>RemoveCommand</code>.
   * @alias module:model/RemoveCommand
   * @class
   * @param token
   */
  var exports = function(token) {

    this['token'] = token;
  };

  /**
   * Constructs a <code>RemoveCommand</code> from a plain JavaScript object, optionally creating a new instance.
   * Copies all relevant properties from <code>data</code> to <code>obj</code> if supplied or a new instance if not.
   * @param {Object} data The plain JavaScript object bearing properties of interest.
   * @param {module:model/RemoveCommand} obj Optional instance to populate.
   * @return {module:model/RemoveCommand} The populated <code>RemoveCommand</code> instance.
   */
  exports.constructFromObject = function(data, obj) {
    if (data) { 
      obj = obj || new exports();

      if (data.hasOwnProperty('token')) {
        obj['token'] = ApiClient.convertToType(data['token'], 'String');
      }
    }
    return obj;
  }


  /**
   * A unique token, that changes after a command is sent from a robot to the server.\nIt is used for recognizing a robot, and chaining the commands, because a robot can not issue another command, before it receive an answer from the server, with the next token to use.\n
   * @member {String} token
   */
  exports.prototype['token'] = undefined;




  return exports;
}));
