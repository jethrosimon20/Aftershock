# 🌪️ Aftershock - Disaster Damage Registry

> 📋 NFT-based incident documentation system for tracking and verifying disaster reports

## 🎯 Overview

Aftershock is a blockchain-based disaster documentation system that creates NFTs for each disaster report. It enables communities to document incidents, track damage, and build a verifiable record of disasters for insurance claims, emergency response, and historical analysis.

## ✨ Features

- 🏷️ **NFT Reports**: Each disaster report is minted as a unique NFT
- ✅ **Verification System**: Authorized verifiers can validate reports
- 📊 **Analytics**: Track statistics by location, disaster type, and reporter
- 🏆 **Reputation System**: Reporters build reputation through verified reports
- 🔒 **Access Control**: Owner-managed verifier permissions
- ⏸️ **Emergency Controls**: Contract pause functionality

## 🚀 Getting Started

### Prerequisites

- Clarinet CLI installed
- Stacks wallet for testing

### Installation

```bash
git clone <your-repo>
cd aftershock
clarinet check
```

## 📖 Usage

### Submit a Disaster Report

```clarity
(contract-call? .aftershock submit-disaster-report 
  "earthquake" 
  "San Francisco, CA" 
  u8 
  "Major earthquake caused building damage" 
  u1000000 
  u5000 
  "emergency@city.gov"
)
```

### Verify a Report (Authorized Verifiers Only)

```clarity
(contract-call? .aftershock verify-report u1)
```

### Transfer Report Ownership

```clarity
(contract-call? .aftershock transfer-report u1 'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7)
```

## 📊 Data Structure

### Report Fields
- **Reporter**: Principal who submitted the report
- **Disaster Type**: Category of disaster (max 50 chars)
- **Location**: Geographic location (max 100 chars)
- **Severity**: Scale of 1-10
- **Description**: Detailed description (max 500 chars)
- **Damage Estimate**: Estimated financial damage
- **Affected Population**: Number of people affected
- **Emergency Contact**: Contact information
- **Status**: Current report status
- **Verification**: Verification status and verifier

## 🔍 Read-Only Functions

- `get-disaster-report`: Retrieve report details
- `get-reporter-stats`: Get reporter statistics
- `get-location-stats`: Get location incident history
- `calculate-risk-score`: Calculate location risk assessment
- `is-verifier`: Check if address is authorized verifier

## 👥 Admin Functions

- `add-verifier`: Add authorized verifier
- `remove-verifier`: Remove verifier permissions
- `pause-contract`: Emergency pause
- `unpause-contract`: Resume operations

## 🏗️ Architecture

The contract uses several data maps to organize information:
- `disaster-reports`: Core report data
- `reporter-stats`: Reporter reputation tracking
- `location-incidents`: Geographic incident tracking
- `disaster-type-stats`: Disaster category analytics
- `authorized-verifiers`: Access control

## 🧪 Testing

```bash
clarinet test
```

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests
5. Submit a pull request

## 📄 License

MIT License - see LICENSE file for details

## 🆘 Support


