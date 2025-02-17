
From Coq Require Export String.


(* following Section V. of the paper: https://doi.org/10.1109/ICAASE64542.2024.10850926 *)

Definition Topic := string.

Record Message := {
  topic : Topic; (* Subject/topic of message. *)
  payload : string; (* Actual data being sent. *)
  qos : nat; (* Quality of Services level: 0, 1, or 2. *)
}.

Record Subscriber := {
  subscriber_id : string;
  subscriber_connected : bool;
  subscriptions : list Topic;
}.

Record Publisher := {
  publisher_id : string;
  publisher_connected : bool;
}.

Record Broker := {
  subscribers : list Subscriber;
  publishers : list Publisher;
  retained_messages : list (Topic * Message);
}.

Inductive Packet :=
  | CONNECT | CONNACK
  | PUBLISH (topic:string) (payload:string) (qos:nat) | PUBACK
  | SUBSCRIBE (topic:string) | SUBACK
  | UNSUBSCRIBE (topic:string) | UNSUBACK
  | DISCONNECT
  | PUBREC
  | PUBREL
  | PUBCOMP.

Inductive PublisherState :=
  | ConnectedPub | ConnectingPub | Publishing | DisconnectedPub.

Inductive SubscriberState :=
  | ConnectedSub | ConnectingSub | Subscribing | Unsubscribing | Receiving | DisconnectedSub.

Inductive BrokerState :=
  | Idle | ProcessingConnect | ReceivingPublish | ManagingSubscriber | ProcessingUnsubscriber | DisconnectedClient.

Definition publisher_transition (ps:PublisherState) (m:Packet) : option PublisherState :=
  match ps, m with
  | DisconnectedPub, CONNECT =>       Some ConnectingPub
  | ConnectingPub,   CONNACK =>       Some ConnectedPub
  | ConnectedPub,    PUBLISH _ _ _ => Some Publishing
  | Publishing,      PUBACK =>        Some ConnectedPub
  | ConnectedPub,    DISCONNECT =>    Some DisconnectedPub
  | _, _ => None
  end.

(* not given in paper, reconstructed from table II, page 4. *)
Definition subscriber_transition (ss:SubscriberState) (m:Packet) : option SubscriberState :=
  match ss, m with
  | DisconnectedSub, CONNECT =>       Some ConnectingSub
  | ConnectingSub,   CONNACK =>       Some ConnectedSub
  | ConnectedSub,    SUBSCRIBE _ =>   Some Subscribing
  | Subscribing,     SUBACK =>        Some ConnectedSub
  | ConnectedSub,    PUBLISH _ _ _ => Some Receiving
  | Receiving,       PUBACK =>        Some ConnectedSub
  | ConnectedSub,    UNSUBSCRIBE _ => Some Unsubscribing
  | Unsubscribing,   UNSUBACK =>      Some ConnectedSub
  | ConnectedSub,    DISCONNECT =>    Some DisconnectedSub
  | _, _ => None
  end.

(* not given in paper, reconstructed from table II, page 4. *)
Definition broker_transition (bs:BrokerState) (m:Packet) : option BrokerState :=
  match bs, m with
  | Idle,                   CONNECT =>       Some ProcessingConnect
  | ProcessingConnect,      CONNACK =>       Some Idle
  | Idle,                   PUBLISH _ _ _ => Some ReceivingPublish
  | Idle,                   SUBSCRIBE _ =>   Some ManagingSubscriber
  | ReceivingPublish,       PUBACK =>        Some Idle
  | ManagingSubscriber,     SUBACK =>        Some Idle
  | Idle,                   UNSUBSCRIBE _ => Some ProcessingUnsubscriber
  | ProcessingUnsubscriber, UNSUBACK =>      Some Idle
  | Idle,                   DISCONNECT =>    Some DisconnectedClient
  | _, _ => None
  end.

Theorem Safety_Property:
  forall (ps:PublisherState) (m:Packet),
    publisher_transition ps m = Some Publishing
      -> ps = ConnectedPub.
Proof.
  intros ps m H. (* this line appears to crash my VSCoq language server? *)
  destruct ps; simpl in H.
  - reflexivity.
  - destruct m; simpl in H; try discriminate.
  - destruct m; simpl in H; try discriminate.
  - destruct m; simpl in H; try discriminate.
Qed.


Theorem QoS1_related_property:
  forall (ps:PublisherState) (bs:BrokerState) (ss:SubscriberState) (m:Packet),
    publisher_transition ps m = Some Publishing
      -> broker_transition bs m = Some ReceivingPublish
        -> exists (m_ack:Packet),
            m_ack = PUBACK
            /\ publisher_transition Publishing m_ack = Some ConnectedPub
            /\ broker_transition ReceivingPublish m_ack = Some Idle
            /\ exists (ss':SubscriberState),
              subscriber_transition ConnectedSub (PUBLISH "t" "p" 1) = Some Receiving
              /\ subscriber_transition Receiving PUBACK = Some ConnectedSub.
Proof. (* this entire proof breaks my VSCoq language server. *)
  intros ps bs ss mm Hpub Hbroker.
  exists PUBACK.
  split.
  - reflexivity.
  - split.
    + reflexivity.
    + split.
      * reflexivity.
      * exists Receiving.
  split; [reflexivity |].
  reflexivity.
Qed.
