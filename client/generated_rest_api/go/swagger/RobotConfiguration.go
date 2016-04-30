package swagger

import (
)

type RobotConfiguration struct {
    Name  string  `json:"name,omitempty"`
    MaxHitPoints  float32  `json:"maxHitPoints,omitempty"`
    MaxSpeed  float32  `json:"maxSpeed,omitempty"`
    Acceleration  float32  `json:"acceleration,omitempty"`
    Decelleration  float32  `json:"decelleration,omitempty"`
    MaxSterlingSpeed  float32  `json:"maxSterlingSpeed,omitempty"`
    MaxScanDistance  float32  `json:"maxScanDistance,omitempty"`
    MaxFireDistance  float32  `json:"maxFireDistance,omitempty"`
    BulletSpeed  float32  `json:"bulletSpeed,omitempty"`
    BulletDamage  float32  `json:"bulletDamage,omitempty"`
    FireReloadingTime  float32  `json:"fireReloadingTime,omitempty"`
    
}
