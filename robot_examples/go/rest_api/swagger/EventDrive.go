package swagger

import (
)

type EventDrive struct {
    EventType  Number  `json:"eventType,omitempty"`
    ActivationTime  float32  `json:"activationTime,omitempty"`
    Robot  RobotInfo  `json:"robot,omitempty"`
    
}
