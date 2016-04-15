package swagger

import (
)

type Event struct {
    EventType  Number  `json:"eventType,omitempty"`
    ActivationTime  float32  `json:"activationTime,omitempty"`
    
}
