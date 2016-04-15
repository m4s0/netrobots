package swagger

import (
)

type BoardInfo struct {
    MaxBoardX  float32  `json:"maxBoardX,omitempty"`
    MaxBoardY  float32  `json:"maxBoardY,omitempty"`
    StreamDelay  float32  `json:"streamDelay,omitempty"`
    TurnDeltaTime  float32  `json:"turnDeltaTime,omitempty"`
    NetworkLatency  float32  `json:"networkLatency,omitempty"`
    StartTime  float32  `json:"startTime,omitempty"`
    EndTime  float32  `json:"endTime,omitempty"`
    Events  []Event  `json:"events,omitempty"`
    
}
