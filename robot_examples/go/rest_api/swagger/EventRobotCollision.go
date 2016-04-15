package swagger

import (
)

type EventRobotCollision struct {
    EventType  Number  `json:"eventType,omitempty"`
    ActivationTime  float32  `json:"activationTime,omitempty"`
    Robot  RobotInfo  `json:"robot,omitempty"`
    
}
