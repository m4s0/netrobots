package swagger

import (
)

type EventMissile struct {
    EventType  Number  `json:"eventType,omitempty"`
    ActivationTime  float32  `json:"activationTime,omitempty"`
    Robot  RobotInfo  `json:"robot,omitempty"`
    Direction  float32  `json:"direction,omitempty"`
    Distance  float32  `json:"distance,omitempty"`
    Speed  float32  `json:"speed,omitempty"`
    
}
