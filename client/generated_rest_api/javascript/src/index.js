(function(factory) {
  if (typeof define === 'function' && define.amd) {
    // AMD. Register as an anonymous module.
    define(['./ApiClient', './model/DriveCommand', './model/FireCommand', './model/RemoveCommand', './model/RobotCommand', './model/RobotConfiguration', './model/RobotStatus', './model/ScanCommand', './model/ScanStatus', './api/DefaultApi'], factory);
  } else if (typeof module === 'object' && module.exports) {
    // CommonJS-like environments that support module.exports, like Node.
    module.exports = factory(require('./ApiClient'), require('./model/DriveCommand'), require('./model/FireCommand'), require('./model/RemoveCommand'), require('./model/RobotCommand'), require('./model/RobotConfiguration'), require('./model/RobotStatus'), require('./model/ScanCommand'), require('./model/ScanStatus'), require('./api/DefaultApi'));
  }
}(function(ApiClient, DriveCommand, FireCommand, RemoveCommand, RobotCommand, RobotConfiguration, RobotStatus, ScanCommand, ScanStatus, DefaultApi) {
  'use strict';

  /**
   * NetRobots REST API specification, using Swagger format..<br>
   * The <code>index</code> module provides access to constructors for all the classes which comprise the public API.
   * <p>
   * An AMD (recommended!) or CommonJS application will generally do something equivalent to the following:
   * <pre>
   * var NetRobotsApi = require('./index'); // See note below*.
   * var xxxSvc = new NetRobotsApi.XxxApi(); // Allocate the API class we're going to use.
   * var yyyModel = new NetRobotsApi.Yyy(); // Construct a model instance.
   * yyyModel.someProperty = 'someValue';
   * ...
   * var zzz = xxxSvc.doSomething(yyyModel); // Invoke the service.
   * ...
   * </pre>
   * <em>*NOTE: For a top-level AMD script, use require(['./index'], function(){...}) and put the application logic within the
   * callback function.</em>
   * </p>
   * <p>
   * A non-AMD browser application (discouraged) might do something like this:
   * <pre>
   * var xxxSvc = new NetRobotsApi.XxxApi(); // Allocate the API class we're going to use.
   * var yyy = new NetRobotsApi.Yyy(); // Construct a model instance.
   * yyyModel.someProperty = 'someValue';
   * ...
   * var zzz = xxxSvc.doSomething(yyyModel); // Invoke the service.
   * ...
   * </pre>
   * </p>
   * @module index
   * @version 2.0
   */
  var exports = {
    /**
     * The ApiClient constructor.
     * @property {module:ApiClient}
     */
    ApiClient: ApiClient,
    /**
     * The DriveCommand model constructor.
     * @property {module:model/DriveCommand}
     */
    DriveCommand: DriveCommand,
    /**
     * The FireCommand model constructor.
     * @property {module:model/FireCommand}
     */
    FireCommand: FireCommand,
    /**
     * The RemoveCommand model constructor.
     * @property {module:model/RemoveCommand}
     */
    RemoveCommand: RemoveCommand,
    /**
     * The RobotCommand model constructor.
     * @property {module:model/RobotCommand}
     */
    RobotCommand: RobotCommand,
    /**
     * The RobotConfiguration model constructor.
     * @property {module:model/RobotConfiguration}
     */
    RobotConfiguration: RobotConfiguration,
    /**
     * The RobotStatus model constructor.
     * @property {module:model/RobotStatus}
     */
    RobotStatus: RobotStatus,
    /**
     * The ScanCommand model constructor.
     * @property {module:model/ScanCommand}
     */
    ScanCommand: ScanCommand,
    /**
     * The ScanStatus model constructor.
     * @property {module:model/ScanStatus}
     */
    ScanStatus: ScanStatus,
    /**
     * The DefaultApi service constructor.
     * @property {module:api/DefaultApi}
     */
    DefaultApi: DefaultApi
  };

  return exports;
}));
