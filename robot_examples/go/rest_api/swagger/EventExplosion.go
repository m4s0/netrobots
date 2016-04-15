package swagger

import (
)

type EventExplosion struct {
    EventType  Number  `json:"eventType,omitempty"`
    ActivationTime  float32  `json:"activationTime,omitempty"`
    Robot  RobotInfo  `json:"robot,omitempty"`
    HitRobot  RobotInfo  `json:"hitRobot,omitempty"`
    Damage  float32  `json:"damage,omitempty"`
    
}
