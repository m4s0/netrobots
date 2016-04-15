package swagger

import (
)

type EventScan struct {
    EventType  Number  `json:"eventType,omitempty"`
    ActivationTime  float32  `json:"activationTime,omitempty"`
    Direction  float32  `json:"direction,omitempty"`
    Semiaperture  float32  `json:"semiaperture,omitempty"`
    ScanMaxDistance  float32  `json:"scanMaxDistance,omitempty"`
    Robot  RobotInfo  `json:"robot,omitempty"`
    HitRobot  RobotInfo  `json:"hitRobot,omitempty"`
    
}
