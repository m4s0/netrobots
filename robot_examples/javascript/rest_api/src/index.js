(function(factory) {
  if (typeof define === 'function' && define.amd) {
    // AMD. Register as an anonymous module.
    define(['./ApiClient', './model/BoardInfo', './model/DriveCommand', './model/Event', './model/EventCreateRobot', './model/EventDrive', './model/EventExplosion', './model/EventMissile', './model/EventRemoveRobot', './model/EventRobotCollision', './model/EventScan', './model/FireCommand', './model/RobotCommand', './model/RobotConfiguration', './model/RobotInfo', './model/RobotStatus', './model/ScanCommand', './model/ScanStatus', './api/DefaultApi'], factory);
  } else if (typeof module === 'object' && module.exports) {
    // CommonJS-like environments that support module.exports, like Node.
    module.exports = factory(require('./ApiClient'), require('./model/BoardInfo'), require('./model/DriveCommand'), require('./model/Event'), require('./model/EventCreateRobot'), require('./model/EventDrive'), require('./model/EventExplosion'), require('./model/EventMissile'), require('./model/EventRemoveRobot'), require('./model/EventRobotCollision'), require('./model/EventScan'), require('./model/FireCommand'), require('./model/RobotCommand'), require('./model/RobotConfiguration'), require('./model/RobotInfo'), require('./model/RobotStatus'), require('./model/ScanCommand'), require('./model/ScanStatus'), require('./api/DefaultApi'));
  }
}(function(ApiClient, BoardInfo, DriveCommand, Event, EventCreateRobot, EventDrive, EventExplosion, EventMissile, EventRemoveRobot, EventRobotCollision, EventScan, FireCommand, RobotCommand, RobotConfiguration, RobotInfo, RobotStatus, ScanCommand, ScanStatus, DefaultApi) {
  'use strict';

  /**
   * NetRobots API.<br>
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
   * @version 2.0.0
   */
  var exports = {
    /**
     * The ApiClient constructor.
     * @property {module:ApiClient}
     */
    ApiClient: ApiClient,
    /**
     * The BoardInfo model constructor.
     * @property {module:model/BoardInfo}
     */
    BoardInfo: BoardInfo,
    /**
     * The DriveCommand model constructor.
     * @property {module:model/DriveCommand}
     */
    DriveCommand: DriveCommand,
    /**
     * The Event model constructor.
     * @property {module:model/Event}
     */
    Event: Event,
    /**
     * The EventCreateRobot model constructor.
     * @property {module:model/EventCreateRobot}
     */
    EventCreateRobot: EventCreateRobot,
    /**
     * The EventDrive model constructor.
     * @property {module:model/EventDrive}
     */
    EventDrive: EventDrive,
    /**
     * The EventExplosion model constructor.
     * @property {module:model/EventExplosion}
     */
    EventExplosion: EventExplosion,
    /**
     * The EventMissile model constructor.
     * @property {module:model/EventMissile}
     */
    EventMissile: EventMissile,
    /**
     * The EventRemoveRobot model constructor.
     * @property {module:model/EventRemoveRobot}
     */
    EventRemoveRobot: EventRemoveRobot,
    /**
     * The EventRobotCollision model constructor.
     * @property {module:model/EventRobotCollision}
     */
    EventRobotCollision: EventRobotCollision,
    /**
     * The EventScan model constructor.
     * @property {module:model/EventScan}
     */
    EventScan: EventScan,
    /**
     * The FireCommand model constructor.
     * @property {module:model/FireCommand}
     */
    FireCommand: FireCommand,
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
     * The RobotInfo model constructor.
     * @property {module:model/RobotInfo}
     */
    RobotInfo: RobotInfo,
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
