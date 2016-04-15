package swagger

import (
)

type RobotCommand struct {
    Token  string  `json:"token,omitempty"`
    Fire  FireCommand  `json:"fire,omitempty"`
    Drive  DriveCommand  `json:"drive,omitempty"`
    Scan  ScanCommand  `json:"scan,omitempty"`
    
}
