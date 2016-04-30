(function(root, factory) {
  if (typeof define === 'function' && define.amd) {
    // AMD. Register as an anonymous module.
    define(['../ApiClient', './DriveCommand', './FireCommand', './ScanCommand'], factory);
  } else if (typeof module === 'object' && module.exports) {
    // CommonJS-like environments that support module.exports, like Node.
    module.exports = factory(require('../ApiClient'), require('./DriveCommand'), require('./FireCommand'), require('./ScanCommand'));
  } else {
    // Browser globals (root is window)
    if (!root.NetRobotsApi) {
      root.NetRobotsApi = {};
    }
    root.NetRobotsApi.RobotCommand = factory(root.NetRobotsApi.ApiClient, root.NetRobotsApi.DriveCommand, root.NetRobotsApi.FireCommand, root.NetRobotsApi.ScanCommand);
  }
}(this, function(ApiClient, DriveCommand, FireCommand, ScanCommand) {
  'use strict';

  /**
   * The RobotCommand model module.
   * @module model/RobotCommand
   * @version 2.0
   */

  /**
   * Constructs a new <code>RobotCommand</code>.
   * A Robot can make 3 actions contemporary: drive, scan, fire. If an action can not be performed, it will be ignored. If the drive action is not specified, the robot will continue moving according the last drive command.
   * @alias module:model/RobotCommand
   * @class
   * @param token
   */
  var exports = function(token) {

    this['token'] = token;



  };

  /**
   * Constructs a <code>RobotCommand</code> from a plain JavaScript object, optionally creating a new instance.
   * Copies all relevant properties from <code>data</code> to <code>obj</code> if supplied or a new instance if not.
   * @param {Object} data The plain JavaScript object bearing properties of interest.
   * @param {module:model/RobotCommand} obj Optional instance to populate.
   * @return {module:model/RobotCommand} The populated <code>RobotCommand</code> instance.
   */
  exports.constructFromObject = function(data, obj) {
    if (data) { 
      obj = obj || new exports();

      if (data.hasOwnProperty('token')) {
        obj['token'] = ApiClient.convertToType(data['token'], 'String');
      }
      if (data.hasOwnProperty('fire')) {
        obj['fire'] = FireCommand.constructFromObject(data['fire']);
      }
      if (data.hasOwnProperty('drive')) {
        obj['drive'] = DriveCommand.constructFromObject(data['drive']);
      }
      if (data.hasOwnProperty('scan')) {
        obj['scan'] = ScanCommand.constructFromObject(data['scan']);
      }
    }
    return obj;
  }


  /**
   * A unique token, that changes after a command is sent from a robot to the server.\nIt is used for recognizing a robot, and chaining the commands, because a robot can not issue another command, before it receive an answer from the server, with the next token to use.\n
   * @member {String} token
   */
  exports.prototype['token'] = undefined;

  /**
   * @member {module:model/FireCommand} fire
   */
  exports.prototype['fire'] = undefined;

  /**
   * @member {module:model/DriveCommand} drive
   */
  exports.prototype['drive'] = undefined;

  /**
   * @member {module:model/ScanCommand} scan
   */
  exports.prototype['scan'] = undefined;




  return exports;
}));
