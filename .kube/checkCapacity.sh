#!/bin/bash
kubectl describe nodes | sed -n '/ProviderID:/,/Events:/p'
