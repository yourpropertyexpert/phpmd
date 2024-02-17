<?php
/**
 * This file is part of PHP Mess Detector.
 *
 * Copyright (c) Manuel Pichler <mapi@phpmd.org>.
 * All rights reserved.
 *
 * Licensed under BSD License
 * For full copyright and license information, please see the LICENSE file.
 * Redistributions of files must retain the above copyright notice.
 *
 * @author Manuel Pichler <mapi@phpmd.org>
 * @copyright Manuel Pichler. All rights reserved.
 * @license https://opensource.org/licenses/bsd-license.php BSD License
 * @link http://phpmd.org/
 */

namespace PHPMD\Rule\Naming;

use PHPMD\AbstractNode;
use PHPMD\AbstractRule;
use PHPMD\Rule\ClassAware;
use PHPMD\Rule\FunctionAware;
use PHPMD\Rule\MethodAware;
use PHPMD\Rule\TraitAware;
use PHPMD\RuleProperty\Option;
use PHPMD\Utility\Strings;

/**
 * This rule class will detect variables, parameters and properties with really
 * long names.
 */
class LongVariable extends AbstractRule implements ClassAware, MethodAware, FunctionAware, TraitAware
{
    /**
     * Temporary cache of configured prefixes to subtract
     *
     * @var string[]
     */
    #[Option]
    public array $subtractPrefixes;

    /**
     * Temporary cache of configured suffixes to subtract
     *
     * @var string[]
     */
    #[Option]
    public array $subtractSuffixes;

    /**
     * Temporary map holding variables that were already processed in the
     * current context.
     *
     * @var array<string, bool>
     */
    protected array $processedVariables = [];

    /**
     * Extracts all variable and variable declarator nodes from the given node
     * and checks the variable name length against the configured maximum
     * length.
     */
    public function apply(AbstractNode $node): void
    {
        $this->resetProcessed();

        if ($node->getType() === 'class') {
            $fields = $node->findChildrenOfType('FieldDeclaration');

            foreach ($fields as $field) {
                if ($field->hasSuppressWarningsAnnotationFor($this)) {
                    continue;
                }

                foreach ($field->findChildrenOfType('VariableDeclarator') as $declarator) {
                    $this->checkNodeImage($declarator);
                }
            }

            $this->resetProcessed();

            return;
        }

        foreach ($node->findChildrenOfType('VariableDeclarator') as $declarator) {
            $this->checkNodeImage($declarator);
        }

        foreach ($node->findChildrenOfTypeVariable() as $variable) {
            $this->checkNodeImage($variable);
        }

        $this->resetProcessed();
    }

    /**
     * Checks if the variable name of the given node is smaller/equal to the
     * configured threshold.
     *
     * @param \PHPMD\AbstractNode $node
     * @return void
     */
    protected function checkNodeImage(AbstractNode $node)
    {
        if ($this->isNotProcessed($node)) {
            $this->addProcessed($node);
            $this->checkMaximumLength($node);
        }
    }

    /**
     * Template method that performs the real node image check.
     *
     * @SuppressWarnings(PHPMD.LongVariable)
     */
    protected function checkMaximumLength(AbstractNode $node): void
    {
        if ($node->hasSuppressWarningsAnnotationFor($this)) {
            return;
        }

        $threshold = $this->getIntProperty('maximum');
        $variableName = $node->getImage();

        $lengthWithoutDollarSign = Strings::lengthWithoutPrefixesAndSuffixes(
            \ltrim($variableName, '$'),
            $this->getSubtractPrefixList(),
            $this->getSubtractSuffixList()
        );

        if ($lengthWithoutDollarSign <= $threshold) {
            return;
        }

        if ($this->isNameAllowedInContext($node)) {
            return;
        }

        $this->addViolation($node, [$variableName, $threshold]);
    }

    /**
     * Checks if a short name is acceptable in the current context. For the
     * moment the only context is a static member.
     */
    protected function isNameAllowedInContext(AbstractNode $node): bool
    {
        return $this->isChildOf($node, 'MemberPrimaryPrefix');
    }

    /**
     * Checks if the given node is a direct or indirect child of a node with
     * the given type.
     */
    protected function isChildOf(AbstractNode $node, string $type): bool
    {
        $parent = $node->getParent();
        while (\is_object($parent)) {
            if ($parent->isInstanceOf($type)) {
                return true;
            }
            $parent = $parent->getParent();
        }

        return false;
    }

    /**
     * Resets the already processed nodes.
     */
    protected function resetProcessed(): void
    {
        $this->processedVariables = [];
    }

    /**
     * Flags the given node as already processed.
     */
    protected function addProcessed(AbstractNode $node): void
    {
        $this->processedVariables[$node->getImage()] = true;
    }

    /**
     * Checks if the given node was already processed.
     */
    protected function isNotProcessed(AbstractNode $node): bool
    {
        return !isset($this->processedVariables[$node->getImage()]);
    }

    /**
     * Gets array of suffixes from property
     *
     * @return string[]
     */
    protected function getSubtractPrefixList(): array
    {
        $this->subtractPrefixes ??= Strings::splitToList($this->getStringProperty('subtract-prefixes', ''), ',');

        return $this->subtractPrefixes;
    }

    /**
     * Gets array of suffixes from property
     *
     * @return string[]
     */
    protected function getSubtractSuffixList(): array
    {
        $this->subtractSuffixes ??= Strings::splitToList($this->getStringProperty('subtract-suffixes', ''), ',');

        return $this->subtractSuffixes;
    }
}
