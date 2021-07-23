---
layout: post
title: "Label Contractor for improving readability of a visualization"
subtitle: In this post I am going to show you how to contract labels in a visualization using the Label Contractor
date: 2021-07-23 16:30:28 -0400
background: '/img/posts/bg-posts.jpg'
author: Réda Id-taleb
---

# **Introduction** 
The visualization construction is not always optimal. In particular, the visualization can contain elements with long names, which poses clarity problems or sometimes a very spread out visualizations, making them difficult to read. If you have to zoom in to read the labels in the visualization, it loses interest in synthesizing the information.

The Label Contractor project comes to solve this problem. 

In this blog post, I will explain how you can apply a reduction following different strategies and how you can combine them.  

# **Label Contractor Description**
The idea was to build a "tool" that can reduce labels without losing informations, and the easiest and most obvious way is to provide the user with a set of strategies, allowing him to apply them separately or in combination.

# **Implementation choices**
First of all, before starting talking about the implemented strategies, I decided that the contractor should take into account the reduction of labels representing the full qualified filename. For that, I have chosen to remove the "path" part by default. 

Example:
```Smalltalk
LbCContractor new 
	reduce: '/home/idtaleb/Label Contractor/images/src/LbCContractor.st'
```
---> LbCContractor.st  

So removing the "path" part is not considered a strategy(maybe it will be in the future, we'll see ...), but it is a behavior common to all strategies.

Notice that you can still keep the "path" part. 

To keep path use #keepPath:

```Smalltalk
LbCContractor new 
    keepPath;
	reduce: '/home/idtaleb/Label Contractor/images/src/LbCContractor.st'
```

One more thing, in some strategies, I supposed that the labels contain words respecting the Camel Case, so a tokenizer was essential to detect each word of labels.

# **Description of the contraction strategies**
Currently there is 13 strategies and in what follow i will explain what each strategy can do and how to apply it.

## **Remove Filename Extension**
remove extension of a filename. The extension is the part after the last dot('.')

```Smalltalk
LbCContractor new 
	reduce: 'ClyMergedSuperclassesAndInheritedTraitsHierarchyTest.st'
```
## **Remove Substrings**
It is a hierarchy of strategies which removes the words of a label.

Notice that by default there's no case sensitive when you enter the words, but you can "activate" the case sensitive.

### **Remove Any Substrings**
This strategy accepts one or a collection of the substring to be removed, and it removes all the occurrences of these substrings and only the existing words in the label.

Example with only one substring to remove:
```Smalltalk
LbCContractor new
 removeSubstring: 'merged';
 reduce: 'ClyMergedSuperclassesAndInheritedTraitsHierarchyTest'.  
```
	--> ClySuperclassesAndInheritedTraitsHierarchyTest

Example with a collection of substrings:
```Smalltalk
LbCContractor new
 removeSubstring: #('cly' 'merged' 'and' 'test');
 reduce: 'ClyMergedSuperclassesAndInheritedTraitsHierarchyTest'.  
```
	--> 'SuperclassesInheritedTraitsHierarchy'

### **Remove Prefix**
The same idea, but the word to be removed must be a prefix. In addition, if a collection of words is given, we remove only the word that is the prefix of a label.

### **Remove Suffix**
This strategy is similar to the last one, except that we remove the suffix words.

## **Remove Words**
It's very similar to the [Remove Substrings](#Remove-Substrings) hierarchy, except that the words to be removed must be specified by its indexes(supposing that the Camel Case is respected). Also you can give an index or a collection of indexes.

### **Remove Any Words**
The same as [Remove Any Substrings](#Remove-Any-Substrings), you can give an index or a collection of indexes of the words you want to remove.

### **Remove First Word**
Removes automatically the first word

### **Remove Last Word**
Removes automatically the last word

## **Susbtitute Substring**
Allow you to replace a word by another one. If the word appears more than once, then all occurrences of the word will be replaced.

Example:
```Smalltalk
LbCContractor new 
 substitute: 'Superclasses' by: 'Sc';
 reduce: 'ClyMergedSuperclassesAndInheritedTraitsHierarchyTest'  
```
	--> 'ClyMergedScAndInheritedTraitsHierarchyTest'

## **Abreviate Names**
Abbreviate the names of the label to its first capital letters. The last name is never abbreviated.

By default, the 3 first names are abreviated:
```Smalltalk
LbCContractor new
 abbreviateNames;
 reduce: 'ClyMergedSuperclassesAndInheritedTraitsHierarchyTest'.
```
	--> CMSAndInheritedTraitsHierarchyTest

## **Remove Vowels**
Sometimes removing the vowels from a word still makes it understandable. So that's why this strategy exists.
Notice that the first letter  of a word is always kept whether it is a vowel or a consonant.

Note: In English, the letter 'y' is sometimes a vowel and sometimes a consonant, so it is only removed if it represents a vowel (according to the grammar rules of the language).

```Smalltalk
LbCContractor new
 removeVowels;
 reduce: 'ClyMergedSuperclassesAndInheritedTraitsHierarchyTest'.
```
	--> ClMrgdSprclsssAndInhrtdTrtsHrrchTst

## **Size Reduction Strategies**
It's a hierarchy of 3 strategies that reduce labels until having a size specified by the user. So the user specifies the size to which the label will be reduced, and the label is reduced according to the chosen strategy.

By default, I have chosen to reduce the labels to a size of 8.

### **Remove Frequent Letters**
It removes the frequent letters until having the size specified by the user.

```Smalltalk
LbCContractor new
 removeFrequentLettersUpTo: 12;
 reduce: 'ClyMergedSuperclassesAndInheritedTraitsHierarchyTest'.
```
	--> ClyMgdpcldIhidiHichy

### **Ellipsis**
Keep the beginning and the end and separate them with a '~'. For example, since the default size is 8 so I take the first 4 characters and the last 4 characters and I separate them with '~'.

### **Pick First Letters**
Take the first characters of a label, so the size specified by the user corresponds to the characters to keep.

# **Strategies Combination**
There's 2 ways to combine the strategies, in the both cases the user must provides the strategies: 
- The user provides an order of strategies, so we apply them one by one:
```Smalltalk
LbCContractor new
 ellipsisUpTo: 20;
 removeVowels;
 removeSubstrings: #('Merged' 'Test');
 reduce: 'ClyMergedSuperclassesAndInheritedTraitsHierarchyTest'.
```
	--> ClMrgdS~rrchTst
- Combining following predefined priorities: 

Sometimes you can make the wrong order and therefore you will have an unreasonable result(as in the previous example, the substrings are not removed, because removeVowels was the first strategy which means that the words to be removed are not available anymore :) )

The same example but with priorities:
```Smalltalk
LbCContractor new
 usingPriorities;
 ellipsisUpTo: 20;
 removeVowels;
 removeSubstrings: #('Merged' 'Test');
 reduce: 'ClyMergedSuperclassesAndInheritedTraitsHierarchyTest'.
```
	--> ClSprclsss~dTrtsHrrch
The result is not the same, because the substrings are removed before applying removeVowels strategy(according to the priority system below).

The priority system is defined as(the color green means that the strategy has the highest priority):

<img src="/img/posts/2021-07-23-label-contractor/strategies_priorities.png" width="350"/>

# **Conclusion**
In this post, we have seen how to compact labels in a visualization using the LabelContractor. The goal is to improve the readability of a visualization while retaining the necessary information.

Note that LabelContractor is not just for visualizations, but you can use it whenever you want.































































































