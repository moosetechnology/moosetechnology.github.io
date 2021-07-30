---
layout: post
title: "Label Contractor for shortning labels"
subtitle: In this post I am going to show you how to contract labels to display more information
date: 2021-07-29 16:30:28 -0400
background: '/img/posts/bg-posts.jpg'
author: Réda Id-taleb
---

# Introduction

When there are long labels in a visualization the displayed elements can overlap which renders the visualization very difficult to read, or the elements have to be very spread out (to not overlap)
and then the visualization does not fit in a normal screen or paper.

The Label Contractor project comes to solve this problem by offering several ways to reduce the length of labels (hence its name).

For example:
```Smalltalk
LbCContractor new
 removeVowels;
 reduce: 'MergedSuperClasses'.
```
will return 'MrgdSprClsss' by suppressing all vowels from the label.


In this blog post, I will explain how you can apply a reduction following different strategies and how you can combine them.  

# How to install the project
In order to install this project, on a Pharo 9.0/Moose Suite 9.0 image execute the following script in the Playground:

```Smalltalk
Metacello new
  baseline: 'LabelContractor';
  repository: 'github://reda-idtaleb/LabelContractor/src';
  load
```
### Load full project
The full project includes examples of the application of the LabelContractor on visualizations and on [Spec2](https://github.com/pharo-spec/Spec). Execute the following script in the Playground: 
```Smalltalk
Metacello new
  baseline: 'LabelContractor';
  repository: 'github://reda-idtaleb/LabelContractor/src';
  load: 'full'.
``` 

# Label Contractor Description

The idea was to build a tool that can reduce labels without losing too much information, and is to provide the user with a set of strategies, allowing him to apply them separately or in combination.

There are startegies for: removing some arbitrary substring from labels, removing all vowels, removing fully qualified path names, etc.

## Implementation choices

The contraction of labels is based on two decisions:
- First, filenames are treated by default to remove the full pathname, therefore '/home/idtaleb/Label Contractor/images/src/LbCContractor.st' will be truncated as 'LbCContractor.st'.
If a label is not a filename, this has no effect on it;
- Second, some strategies working on words assume the labels follow the CamelCase convention.

Currently these decisions are hardcoded in the contractor, but they will be implemented as normal strategies in the future.

There are 13 strategies that we are going to review now.

### Remove Filename Extension

This strategy removes the extension of filenames. The extension is the part of the label after the last dot ('.')

```Smalltalk
LbCContractor new
	removeFilenameExtension ;
	reduce: 'LbCContractor.st'
```

will return 'LbCContractor'.

### Abbreviate Names

This strategy abbreviates the words in the label to their first capital letter.
As explained before, the label is assumed to follow the CamelCase convention.
Only the first three words can be abbreviated (if there are more than three words).
On top of that, the last word is not abbreviated.

```Smalltalk
LbCContractor new
 abbreviateNames;
 reduce: 'ClyMergedSuperclassesAndInheritedTraitsHierarchyTest'
```
will return 'CMSAndInheritedTraitsHierarchyTest' (only the first tree words Cly, Merged, and Superclasses were abbreviated).

### Remove Vowels
This strategy removes all vowels from the label.
Notice that the first letter of a word is always kept whether it is a vowel or a consonant.

Note: In English, the letter 'y' is sometimes considered a vowel and sometimes a consonant.
This strategy assumes that 'y' is a consonnant when it is followed by a vowel like in 'layer'.

```Smalltalk
LbCContractor new
 removeVowels;
 reduce: 'ClyMergedSuperclassesAndInheritedTraitsHierarchyTest'
```
will return 'ClMrgdSprclsssAndInhrtdTrtsHrrchTst'.

```Smalltalk
LbCContractor new
 removeVowels;
 reduce: 'layer'
```
will return 'lyr'.

### Susbtitute Substring
This strategy replaces a word by another one. If the word appears more than once, then all occurrences of the word will be replaced.

Example:
```Smalltalk
LbCContractor new 
 substitute: 'Superclasses' by: 'Sc';
 reduce: 'ClyMergedSuperclassesAndInheritedTraitsHierarchyTest'  
```
will return 'ClyMergedScAndInheritedTraitsHierarchyTest'.

## Size Reduction Strategies

There are three strategies based on specifically fixing a maximal size for the contracted label.

### Remove Frequent Letters

This strategy removes the frequent letters until having the maximal size.
The startegy is not case sensitive, meaning that a 'T' is counted as a 't'.

```Smalltalk
LbCContractor new
 removeFrequentLettersUpTo: 20;
 reduce: 'ClyMergedSuperclassesAndInheritedTraitsHierarchyTest'.
```

will return 'ClyMgdpcldIhidiHichy'.

removing the letters (number of apparition in parentheses) 'e' (8), 'r' (6), 's' (6), 'u' (1), 'a' (4), 'n' (2), and 't' (5).

### Ellipsis

This strategy keeps the beginning and the end of the label and replace the middle by ellipsis represented as a '~'. 
The default size is eight, so it keeps the first four characters and the last four characters af the label and separates them with a tilde '~'.
The default size can be changed.

```Smalltalk
LbCContractor new
 ellipsis;
 reduce: 'ClyMergedSuperclassesAndInheritedTraitsHierarchyTest'
```
will return 'ClyM~Test'.

### Pick First Characters

This strategy takes the first eight characters of a label.
Again, the default size can be changed.

```Smalltalk
LbCContractor new
 pickFirstCharacters;
 reduce: 'ClyMergedSuperclassesAndInheritedTraitsHierarchyTest'.
```
will return 'ClyMerge' (the first eight letters are kept)

## Remove Substrings

This is another group of three strategies that remove some given substring from a label.

Notice that by default the startegies are not case sensitive.


### Remove Any Substrings

This strategy accepts one or a collection of substring to be removed, and it removes all the occurrences of these substrings in the label.

An example with only one substring to remove:
```Smalltalk
LbCContractor new
 removeSubstring: 'Merged';
 reduce: 'ClyMergedSuperclassesAndInheritedTraitsHierarchyTest' 
```
will return 'ClySuperclassesAndInheritedTraitsHierarchyTest'. 

An other example with a collection of substrings:
```Smalltalk
LbCContractor new
 removeSubstrings: #('cly' 'merged' 'and' 'test');
 reduce: 'ClyMergedSuperclassesAndInheritedTraitsHierarchyTest' 
```
will return 'SuperclassesInheritedTraitsHierarchy'.

### Remove Prefix

The same idea, this strategy removes the prefix of the label. In addition, if a collection of words is given, it removes only the word that is the prefix of a label.

```Smalltalk
LbCContractor new
 removePrefix: 'Cly';
 reduce: 'ClyMergedSuperclassesAndInheritedTraitsHierarchyTest' 
```
will return 'MergedSuperclassesAndInheritedTraitsHierarchyTest'.

### Remove Suffix

This strategy is similar to the last one, except that it removes the suffix substrings.

## Remove Words At

This is a group of three strategies which is very similar to the [Remove Substrings](#Remove-Substrings) group, except that it removes words in the label (assuming a CamelCase convention).
The words to remove are specified by their indexes.

### Remove Any Words At

This strategy removes words of the label, that are specified by their indexes. 
Like [Remove Any Substrings](#Remove-Any-Substrings), you can give an index or a collection of indexes of the words to remove.

```Smalltalk
LbCContractor new
 removeWordAt: 2;
 reduce: 'ClyMergedSuperclassesAndInheritedTraitsHierarchyTest' 
```
will return 'ClyMergedSuperclassesAndInheritedTraitsHierarchyTest' (the second word, 'Merged' was removed).

### Remove First Word

This strategy removes automatically the first word of the label.

### Remove Last Word

This strategy removes automatically the last word of the label.

## Strategies Combination

Finally, there are two ways to combine the strategies together, in the both cases the user must provides the strategies:

- The user provides the strategies in the order to apply them:
```Smalltalk
LbCContractor new
 ellipsisUpTo: 20;
 removeVowels;
 removeSubstrings: #('Merged' 'Test');
 reduce: 'ClyMergedSuperclassesAndInheritedTraitsHierarchyTest'.
```
will return 'ClMrgdS~rrchTst' by applying first 'ellipsisUpTo:', then 'removeVowels', and then 'removeSubstrings:'.
Note that the last one was actually not applied because the other two had already changed the label, and the ellipsis is shorter than expected because 'removeVowels' came after.

- Combining following predefined priorities: 

To avoid unreasonable result (as in the previous example), the strategies have built-in priorities that can be applied with 'usingPriorities'.

The same example but with priorities:
```Smalltalk
LbCContractor new
 usingPriorities;
 ellipsisUpTo: 20;
 removeVowels;
 removeSubstrings: #('Merged' 'Test');
 reduce: 'ClyMergedSuperclassesAndInheritedTraitsHierarchyTest'.
```
will return 'ClSprclsss~dTrtsHrrch'

The result is different, because the substrings were removed before applying removeVowels strategy which was itself applied before 'ellipsisUpTo:'.

The priority system is defined as follows (the color green means that the strategy has the highest priority):

<img src="/img/posts/2021-07-23-label-contractor/strategies_priorities.png" width="350"/>

# Conclusion

In this post, we have seen how to compact labels in a visualization using the LabelContractor. The goal is to improve the readability of a visualization while retaining as much information as possible.

Note that LabelContractor is not just for visualizations, but you can use it whenever you want.
