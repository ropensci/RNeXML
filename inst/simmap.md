## simmap NeXML definitions 

- Author: Carl Boettiger
- Date: 2014-03-21



Definitions of the `simmap` namespace, as defined for the `RNeXML`

  term               | definition
 ------------------- | -------------
 `reconstructions`   | Indicates a meta block containing one or more stochastic character map reconstructions for a given `nex:edge`
 `reconstruction`    | Indicates the beginning of a reconstruction. Annotates a `reconstructions` element.
 `char`              | The id of a character trait, as defined by the `nex:char` element with this value as its `id`. Annotates a `reconstruction` element.
 `stateChange`       | A meta block indicating the character state assigned to the edge during a specified interval. Must have children `order`, `length`, and `state`.  Annotates a `reconstruction` element.  
`order`              | The chronological order (from the root) in which the state is assigned to the edge.  An edge that does not change states still has `order` 1.   Annotates a `stateChange` element.  
`length`             | The duration for which the edge occupies the assigned state, in the same units as the `nex:length` attribute defined on the parent `nex:edge`.   Annotates a `stateChange` element.  
 `state`             | The id of a state of a character trait, as defined by the `nex:state` element with this value as its `id`.  Annotates a `stateChange` element.  


The `nex:` elements are defined by the [NeXML schema](http://www.nexml.org/2009) 
