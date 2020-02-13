---
title: 'Distributed Ledger Technology and Settlement Latency'
subtitle: 
summary: 'A companion piece to [Hautsch et al (2019)](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3302159) on how latency enters blockchain-based settlement'
authors:
- admin
tags:
- Academic
categories:
- Blockchain
date: "2019-12-03T00:00:00Z"
lastmod: "2019-12-03T00:00:00Z"
featured: true
draft: false

# Featured image
# To use, add an image named `featured.jpg/png` to your page's folder.
# Placement options: 1 = Full column width, 2 = Out-set, 3 = Screen-width
# Focal point options: Smart, Center, TopLeft, Top, TopRight, Left, Right, BottomLeft, Bottom, BottomRight
image:
  placement: 2
  caption: ''
  focal_point: ""
  preview_only: false

# Projects (optional).
#   Associate this post with one or more of your projects.
#   Simply enter your project's folder or file name without extension.
#   E.g. `projects = ["internal-project"]` references `content/project/deep-learning/index.md`.
#   Otherwise, set `projects = []`.
projects: []
---

In its essence, the distributed ledger technology (DLT) is a digital record-keeping system that allows for the verification, updating and storage of transfers of ownership without the need for a designated third party. It relies on a single ledger that is distributed among many different parties who are incentivized to ensure a truthful representation of the transaction history. Nakamoto first popularized the idea of DLTs in a financial context with the Bitcoin protocol and the underlying concept of blockchain. Nowadays, however, Bitcoin is just one of several hundred applications that use the blockchain technology, while other forms of DLTs, in particular directed acyclic graphs (DAG), are actively explored as well. In the following, we first describe the building blocks of DLT before we turn to a more detailed discussion of blockchains and DAGs.

## Fundamentals of Distributed Ledgers

DLT solves the fundamental problems that arise in the context of digital transfer of ownership. Transactions are pieces of information that agents authorize to be sent to other agents. A record-keeping system has to ensure that transactions are signed and recorded in the correct order. In principle, a single authority could verify signatures and consistency of transactions, but it would be prone to failures. As a result, it might be desirable to distribute this type of information to a system with multiple machines that can sustain the failure of single units. A fault-tolerant design would enable a system to continue its operation, even when one or several units stop working.

To achieve this goal, DLT essentially combines two fundamental concepts. First, a distributed ledger is based on *asymmetrical cryptography* that enables digital signatures of transactions. On the one hand, the sender of a transaction wants to be the sole owner of the signature that allows the transfer of assets from her private wealth. On the other hand, a record-keeping system requires information about the identities of the parties involved. Cryptographic algorithms ensure that any private keys are only known to their owners, while public keys may be disseminated widely. That is, everybody can check whether a private key is valid, but nobody can back out the private key from public information.[^1]

Second, a distributed ledger is conceptually a *distributed system* which checks whether transactions should be in the system and in which order. In a distributed system, many machines are connected through a network and ensure that the system keeps operating even when some machines fail or try to mess up the system. For instance, if the sender of a transaction is also a potential validator, then she has an incentive for dishonest behavior, such as double-spending or revoking transactions. Individual machines have to reach some form of consensus about actual transaction histories. This consensus can be achieved through different network structures such as blockchain or DAG. 

## Blockchain

In the context of blockchain, the typical solution to the consensus problem involves competition among potential validators for the right to append information to the ledger.[^2] The most common consensus protocol, Proof-of-Work (PoW), involves solving a computationally expensive problem where the winner gets the right to update the ledger and typically receives a reward. This particular form of DLT is called blockchain since transactions are not verified individually, but rather appended to the ledger in blocks. Validators bundle transactions that wait for verification and try to solve the problem. However, the system's protocol limits the number of transactions that can be included in a single block. This limit leads to a queue of unconfirmed transactions and validators are free to choose the transactions they try to append to the blockchain. Average verification times thus not only depend on the number of unconfirmed transactions, but also on the fee associated with a transaction, as validators find it more attractive to include transactions with high fees in their blocks.

The computationally difficult problem typically relies on cryptographic hash functions, which map an input of arbitrary size to output of fixed size and cannot be inverted. In the Bitcoin network, validators bundle the information of several transactions and a reference to the current state of the blockchain and plug the data into a hash function. The hash function converts this input into a sequence of characters and numbers of certain length. The system's protocol then requires that the output starts with a certain number of zeros. The probability of calculating a hash that starts with many zeros is very low and to generate a new hash, validators include a random number called *nonce* that can lead to a very different output. The difficulty of the problem is then determined by the number of leading zeros validators have to find. Depending on total available computational power, the system regularly adjusts the target to achieve an average of 10 minutes between two consecutive blocks.

While validators in a PoW system utilize substantial computational resources to win the competition for block generation, validators might also be randomly chosen based on their wealth. In the so called Proof-of-Stake (PoS) protocol, validators stake their tokens to be able to create blocks. The higher a validator's stake, the higher are the chances of creating the next block. After successfully appending a new block, the validator receives transaction fees just as in the case of PoW. If the validator submits an incorrect block or is offline during a staking period, then she is penalized and (at least partly) loses her stake. The penalty might either arise explicitly through a deduction of funds from the stake or implicitly as dishonest behavior creates a feedback on the value of the stake. In particular, if a validator appends to the blockchain in a way that perpetuates disagreement, then she imposes a cost upon all users of the particular blockchain. Such behavior lowers the value of the whole network and is also reflected in a lower valuation of the misbehaving validator's stake. The endogenous feedback between validators' behavior and the value of their stakes incentives them to eventually reach consensus.

Other consensus protocols combine features of PoW and PoS. For instance, delegated Proof-of-Stake (DPoS) relies both on stakeholders, who elect validators and have voting rights proportional to their stake, and validators, who exert effort to append information to the ledger. The reputation of validators determines their chance for reelection, while stakeholders have incentives to select truthful validators. 

## Directed Acyclic Graphs

While blockchains record transactions in blocks, DAGs store information in single transactions. More specifically, any transaction represents a node in a graph (i.e., a set of vertices connected by edges) and each new transaction confirms at least one previous transaction, depending on the configuration of the underlying protocol. The longer the branch on which a transaction is based, the more certain is its validity. Intuitively, only once a transaction is broadcasted sufficiently throughout the network, it is verified. DAGs thus hinge on a steady flow of new transactions that enter the network to verify and reference old transactions. The connections between transactions are directed (i.e., the edges in the form of confirmations are one way) and the whole graph is acyclic (i.e., it is impossible to traverse the entire graph starting from a single edge). Given a high number of transactions, DAG ledgers scale better and can achieve consensus faster than blockchains which rely on fixed block sizes and limited verification rates. 

## Settlement Latency in Distributed Systems

A distributed system features settlement latency in transaction verification, as it is ex-ante unclear how long it takes until validators achieve consensus or to broadcast that consensus through the network. For PoW, latency depends on the time it takes for validators to find a solution to the computationally expensive problem. In the Bitcoin protocol, for instance, validators append a new block on average every 10 minutes, while the process takes about 20 seconds in the Ethereum protocol. For PoS, latency depends on how long disagreement on the correct order of transactions persists. For both blockchain-based protocols, the information still needs to be distributed to all other nodes in the network, possibly facing technological limitations that prevent instant percolation. Technological limits are also particularly relevant for DAGs which rely on a large number of nodes that verify transactions and distribute information through the network. Overall, any distributed system that refrains from using designated third-parties bearing the counterparty risk associated with transactions thus features settlement latency. 

[^1]: The most simple illustrative example for asymmetric cryptography is the multiplication of prime numbers. One can easily multiply two prime numbers (private key) to get a large number (public key), but it can be difficult to infer the initial set of numbers from the product.

[^2]: The problem is more severe in a *permissionless* blockchain where anybody can access and potentially update the blockchain. Other variants, where only few institutions or individuals are entitled to direct access to the blockchain, so-called *permissioned* blockchains, limit the problem to few players.
