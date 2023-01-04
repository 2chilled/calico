package calico.html.defs.styles.traits

import calico.html.keys.StyleProp
import calico.html.modifiers.KeySetter.StyleSetter

// #NOTE: GENERATED CODE
//  - This file is generated at compile time from the data in Scala DOM Types
//  - See `project/DomDefsGenerator.scala` for code generation params
//  - Contribute to https://github.com/raquo/scala-dom-types to add missing tags / attrs / props / etc.

trait JustifyContent extends AlignContent { this: StyleProp[_] =>

  lazy val left: StyleSetter = this := "left"

  lazy val right: StyleSetter = this := "right"

}