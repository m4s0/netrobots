package swagger

import (
)

type EventCreateRobot struct {
    EventType  Number  `json:"eventType,omitempty"`
    ActivationTime  float32  `json:"activationTime,omitempty"`
    Robot  RobotInfo  `json:"robot,omitempty"`
    Name  string  `json:"name,omitempty"`
    Color  string  `json:"color,omitempty"`
    
}
