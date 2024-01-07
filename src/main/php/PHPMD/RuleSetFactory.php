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

namespace PHPMD;

use PHPMD\Exception\RuleClassFileNotFoundException;
use PHPMD\Exception\RuleClassNotFoundException;
use PHPMD\Exception\RuleNotFoundException;
use PHPMD\Exception\RuleSetNotFoundException;
use RuntimeException;
use Symfony\Component\Yaml\Yaml;

/**
 * This factory class is used to create the {@link \PHPMD\RuleSet} instance
 * that PHPMD will use to analyze the source code.
 */
class RuleSetFactory
{
    /**
     * Is the strict mode active?
     *
     * @var boolean
     * @since 1.2.0
     */
    private $strict = false;

    /**
     * The data directory set within the class constructor.
     *
     * @var string
     */
    private $location;

    /**
     * The minimum priority for rules to load.
     *
     * @var integer
     */
    private $minimumPriority = Rule::LOWEST_PRIORITY;

    /**
     * The maximum priority for rules to load.
     *
     * @var integer
     */
    private $maximumPriority = Rule::HIGHEST_PRIORITY;

    /**
     * Constructs a new default rule-set factory instance.
     */
    public function __construct()
    {
        $this->location = __DIR__ . '/../../resources';
    }

    /**
     * Activates the strict mode for all rule sets.
     *
     * @return void
     * @since 1.2.0
     */
    public function setStrict()
    {
        $this->strict = true;
    }

    /**
     * Sets the minimum priority that a rule must have.
     *
     * @param integer $minimumPriority The minimum priority value.
     * @return void
     */
    public function setMinimumPriority($minimumPriority)
    {
        $this->minimumPriority = $minimumPriority;
    }

    /**
     * Sets the maximum priority that a rule must have.
     *
     * @param integer $maximumPriority The maximum priority value.
     * @return void
     */
    public function setMaximumPriority($maximumPriority)
    {
        $this->maximumPriority = $maximumPriority;
    }

    /**
     * Creates an array of rule-set instances for the given argument.
     *
     * @param string $ruleSetFileNames Comma-separated string of rule-set filenames or identifier.
     * @return \PHPMD\RuleSet[]
     */
    public function createRuleSets($ruleSetFileNames)
    {
        $ruleSets = [];

        $ruleSetFileName = strtok($ruleSetFileNames, ',');
        while ($ruleSetFileName !== false) {
            $ruleSets[] = $this->createSingleRuleSet($ruleSetFileName);

            $ruleSetFileName = strtok(',');
        }

        return $ruleSets;
    }

    /**
     * Creates a single rule-set instance for the given filename or identifier.
     *
     * @param string $ruleSetOrFileName The rule-set filename or identifier.
     * @return \PHPMD\RuleSet
     */
    public function createSingleRuleSet($ruleSetOrFileName)
    {
        $fileName = $this->createRuleSetFileName($ruleSetOrFileName);

        return $this->parseRuleSetNode($fileName);
    }

    /**
     * Lists available rule-set identifiers.
     *
     * @return string[]
     */
    public function listAvailableRuleSets()
    {
        return array_merge(
            self::listRuleSetsInDirectory($this->location . '/rulesets/'),
            self::listRuleSetsInDirectory(getcwd() . '/rulesets/')
        );
    }

    /**
     * This method creates the filename for a rule-set identifier or it returns
     * the input when it is already a filename.
     *
     * @param string $ruleSetOrFileName The rule-set filename or identifier.
     * @return string Path to rule set file name
     * @throws RuleSetNotFoundException Thrown if no readable file found
     */
    private function createRuleSetFileName($ruleSetOrFileName)
    {
        foreach ($this->filePaths($ruleSetOrFileName) as $filePath) {
            if ($this->isReadableFile($filePath)) {
                return $filePath;
            }
        }

        throw new RuleSetNotFoundException($ruleSetOrFileName);
    }

    /**
     * Lists available rule-set identifiers in given directory.
     *
     * @param string $directory The directory to scan for rule-sets.
     * @return string[]
     */
    private static function listRuleSetsInDirectory($directory)
    {
        $ruleSets = [];
        if (is_dir($directory)) {
            foreach (scandir($directory) as $file) {
                $matches = [];
                if (is_file($directory . $file) && preg_match('/^(.*)\.xml$/', $file, $matches)) {
                    $ruleSets[] = $matches[1];
                }
            }
        }

        return $ruleSets;
    }

    /**
     * This method parses the rule-set definition in the given file.
     *
     * @throws RuntimeException When loading the XML file fails.
     */
    private function parseRuleSetNode(string $fileName): RuleSet
    {
        $format = preg_match('/\.(?<format>php|yaml)(?:\.dist)?$/', $fileName, $match)
            ? $match['format']
            : 'xml';

        return match ($format) {
            'php' => $this->getConfigFromPhpFile($fileName),
            'yaml' => $this->getConfigFromYamlFile($fileName),
            default => $this->getConfigFromXmlFile($fileName),
        };
    }

    /**
     * This method parses a single rule xml node. Bases on the structure of the
     * xml node this method delegates the parsing process to another method in
     * this class.
     *
     * @param \PHPMD\RuleSet $ruleSet
     * @param \SimpleXMLElement $node
     * @return void
     */
    private function parseRuleNode(RuleSet $ruleSet, \SimpleXMLElement $node)
    {
        $ref = (string)$node['ref'];

        if ($ref === '') {
            $this->parseSingleRuleNode($ruleSet, $node);

            return;
        }

        if (preg_match('/\.(?:xml|yaml|php)$/i', $ref)) {
            $this->parseRuleSetReferenceNode($ruleSet, $node);

            return;
        }

        $this->parseRuleReferenceNode($ruleSet, $node);
    }

    /**
     * This method parses a complete rule set that was includes a reference in
     * the currently parsed ruleset.
     *
     * @param \PHPMD\RuleSet $ruleSet
     * @param \SimpleXMLElement $ruleSetNode
     * @return void
     */
    private function parseRuleSetReferenceNode(RuleSet $ruleSet, \SimpleXMLElement $ruleSetNode)
    {
        foreach ($this->parseRuleSetReference($ruleSetNode) as $rule) {
            if ($this->isIncluded($rule, $ruleSetNode)) {
                $ruleSet->addRule($rule);
            }
        }
    }

    /**
     * Parses a rule-set xml file referenced by the given rule-set xml element.
     *
     * @param \SimpleXMLElement $ruleSetNode
     * @return \PHPMD\RuleSet
     * @since 0.2.3
     */
    private function parseRuleSetReference(\SimpleXMLElement $ruleSetNode)
    {
        $ruleSetFactory = new RuleSetFactory();
        $ruleSetFactory->setMinimumPriority($this->minimumPriority);
        $ruleSetFactory->setMaximumPriority($this->maximumPriority);

        return $ruleSetFactory->createSingleRuleSet((string)$ruleSetNode['ref']);
    }

    /**
     * Checks if the given rule is included/not excluded by the given rule-set
     * reference node.
     *
     * @param \PHPMD\Rule $rule
     * @param \SimpleXMLElement $ruleSetNode
     * @return boolean
     * @since 0.2.3
     */
    private function isIncluded(Rule $rule, \SimpleXMLElement $ruleSetNode)
    {
        foreach ($ruleSetNode->exclude as $exclude) {
            if ($rule->getName() === (string)$exclude['name']) {
                return false;
            }
        }

        return true;
    }

    /**
     * This method will create a single rule instance and add it to the given
     * {@link \PHPMD\RuleSet} object.
     *
     * @param \PHPMD\RuleSet $ruleSet
     * @param \SimpleXMLElement $ruleNode
     * @return void
     * @throws RuleClassFileNotFoundException
     * @throws RuleClassNotFoundException
     */
    private function parseSingleRuleNode(RuleSet $ruleSet, \SimpleXMLElement $ruleNode)
    {
        $fileName = '';

        $ruleSetFolderPath = dirname($ruleSet->getFileName());

        if (isset($ruleNode['file'])) {
            if (is_readable((string)$ruleNode['file'])) {
                $fileName = (string)$ruleNode['file'];
            } elseif (is_readable($ruleSetFolderPath . DIRECTORY_SEPARATOR . (string)$ruleNode['file'])) {
                $fileName = $ruleSetFolderPath . DIRECTORY_SEPARATOR . (string)$ruleNode['file'];
            }
        }

        $className = (string)$ruleNode['class'];

        if (!is_readable($fileName)) {
            $fileName = strtr($className, '\\', '/') . '.php';
        }

        if (!is_readable($fileName)) {
            $fileName = str_replace(['\\', '_'], '/', $className) . '.php';
        }

        if (class_exists($className) === false) {
            $handle = @fopen($fileName, 'r', true);
            if ($handle === false) {
                throw new RuleClassFileNotFoundException($className);
            }
            fclose($handle);

            include_once $fileName;

            if (class_exists($className) === false) {
                throw new RuleClassNotFoundException($className);
            }
        }

        /* @var $rule \PHPMD\Rule */
        $rule = new $className();
        $rule->setName((string)$ruleNode['name']);
        $rule->setMessage((string)$ruleNode['message']);
        $rule->setExternalInfoUrl((string)$ruleNode['externalInfoUrl']);

        $rule->setRuleSetName($ruleSet->getName());

        if (isset($ruleNode['since']) && trim($ruleNode['since']) !== '') {
            $rule->setSince((string)$ruleNode['since']);
        }

        $this->parseRuleProperties($rule, $ruleNode);

        if ($rule->getPriority() <= $this->minimumPriority && $rule->getPriority() >= $this->maximumPriority) {
            $ruleSet->addRule($rule);
        }
    }

    /**
     * This method parses a single rule that was included from a different
     * rule-set.
     */
    private function parseRuleReferenceNode(RuleSet $ruleSet, \SimpleXMLElement|\ArrayAccess|array $ruleNode)
    {
        $ref = (string)$ruleNode['ref'];

        ['file' => $fileName, 'rule' => $ruleName] = preg_match('`^(?<file>.*\.(?:xml|yaml|php))/(?<rule>.*)`i', $ref, $matches)
            ? $matches
            : ['file' => '', 'rule' => $ref];
        $fileName = $this->createRuleSetFileName($fileName);

        $ruleSetFactory = new RuleSetFactory();

        $ruleSetRef = $ruleSetFactory->createSingleRuleSet($fileName);
        $rule = $ruleSetRef->getRuleByName($ruleName) ?? throw new RuleNotFoundException($ruleName);

        if (isset($ruleNode['name']) && trim($ruleNode['name']) !== '') {
            $rule->setName((string)$ruleNode['name']);
        }
        if (isset($ruleNode['message']) && trim($ruleNode['message']) !== '') {
            $rule->setMessage((string)$ruleNode['message']);
        }
        if (isset($ruleNode['externalInfoUrl']) && trim($ruleNode['externalInfoUrl']) !== '') {
            $rule->setExternalInfoUrl((string)$ruleNode['externalInfoUrl']);
        }

        $this->parseRuleProperties($rule, $ruleNode);

        if ($rule->getPriority() <= $this->minimumPriority && $rule->getPriority() >= $this->maximumPriority) {
            $ruleSet->addRule($rule);
        }
    }

    private function parseRuleProperties(Rule $rule, \SimpleXMLElement|\ArrayAccess|array $ruleNode): void
    {
        if (isset($ruleNode['description'])) {
            $rule->setDescription((string)$ruleNode['description']);
        }

        if (isset($ruleNode['example'])) {
            $rule->addExample((string)$ruleNode['example']);
        }

        if (isset($ruleNode['priority'])) {
            $rule->setPriority((int)$ruleNode['priority']);
        }

        if (isset($ruleNode['properties'])) {
            $this->parsePropertiesNode($rule, $ruleNode['properties']);
        }

        if (!($ruleNode instanceof \SimpleXMLElement)) {
            return;
        }

        foreach ($ruleNode->children() as $node) {
            if ($node->getName() === 'description') {
                $rule->setDescription((string)$node);
            } elseif ($node->getName() === 'example') {
                $rule->addExample((string)$node);
            } elseif ($node->getName() === 'priority') {
                $rule->setPriority((int)$node);
            } elseif ($node->getName() === 'properties') {
                $this->parsePropertiesNode($rule, $node);
            }
        }
    }

    /**
     * This method parses a xml properties structure and adds all found properties
     * to the given <b>$rule</b> object.
     *
     * <code>
     *   ...
     *   <properties>
     *       <property name="foo" value="42" />
     *       <property name="bar" value="23" />
     *       ...
     *   </properties>
     *   ...
     * </code>
     *
     * @param \PHPMD\Rule $rule
     * @param \SimpleXMLElement $propertiesNode
     * @return void
     */
    private function parsePropertiesNode(Rule $rule, \SimpleXMLElement|array $propertiesNode)
    {
        if (!($propertiesNode instanceof \SimpleXMLElement)) {
            foreach ($propertiesNode as $name => $value) {
                $name = trim($name);

                if ($name !== '') {
                    $rule->addProperty($name, $value);
                }
            }

            return;
        }

        foreach ($propertiesNode->children() as $node) {
            if ($node->getName() === 'property') {
                $this->addProperty($rule, $node);
            }
        }
    }

    /**
     * Adds an additional property to the given <b>$rule</b> instance.
     *
     * @param \PHPMD\Rule $rule
     * @param \SimpleXMLElement $node
     * @return void
     */
    private function addProperty(Rule $rule, \SimpleXMLElement $node)
    {
        $name = trim($node['name']);
        $value = trim($this->getPropertyValue($node));
        if ($name !== '' && $value !== '') {
            $rule->addProperty($name, $value);
        }
    }

    /**
     * Returns the value of a property node. This value can be expressed in
     * two different notations. First version is an attribute named <b>value</b>
     * and the second valid notation is a child element named <b>value</b> that
     * contains the value as character data.
     *
     * @param \SimpleXMLElement $propertyNode
     * @return string
     * @since 0.2.5
     */
    private function getPropertyValue(\SimpleXMLElement $propertyNode)
    {
        if (isset($propertyNode->value)) {
            return (string)$propertyNode->value;
        }

        return (string)$propertyNode['value'];
    }

    /**
     * Returns an array of path exclude patterns in format described at
     *
     * http://pmd.sourceforge.net/pmd-5.0.4/howtomakearuleset.html#Excluding_files_from_a_ruleset
     *
     * @param string $fileName The filename of a rule-set definition.
     * @return array|null
     * @throws RuntimeException Thrown if file is not proper xml
     */
    public function getIgnorePattern($fileName)
    {
        $excludes = [];
        foreach (array_map('trim', explode(',', $fileName)) as $ruleSetFileName) {
            $ruleSetFileName = $this->createRuleSetFileName($ruleSetFileName);

            // Hide error messages
            $libxml = libxml_use_internal_errors(true);

            $xml = simplexml_load_string(file_get_contents($ruleSetFileName));
            if ($xml === false) {
                // Reset error handling to previous setting
                libxml_use_internal_errors($libxml);

                throw new RuntimeException(trim(libxml_get_last_error()->message));
            }

            foreach ($xml->children() as $node) {
                if ($node->getName() === 'exclude-pattern') {
                    $excludes[] = '' . $node;
                }
            }

            return $excludes;
        }

        return null;
    }

    /**
     * Checks if given file path exists, is file (or symlink to file)
     * and is readable by current user
     *
     * @param string $filePath File path to check against
     * @return bool True if file exists and is readable, false otherwise
     */
    private function isReadableFile($filePath)
    {
        return is_readable($filePath) && is_file($filePath);
    }

    /**
     * Returns list of possible file paths to search against code rules
     *
     * @param string $fileName Rule set file name
     * @return array Array of possible file locations
     */
    private function filePaths(string $fileName): array
    {
        $filePathParts = [
            [$fileName],
            [$this->location, $fileName],
            [$this->location, 'rulesets', $fileName . '.xml'],
            [getcwd(), 'rulesets', $fileName . '.xml'],
        ];

        foreach (explode(PATH_SEPARATOR, get_include_path()) as $includePath) {
            $filePathParts[] = [$includePath, $fileName];
            $filePathParts[] = [$includePath, $fileName . '.xml'];
        }

        return array_map('implode', array_fill(0, \count($filePathParts), DIRECTORY_SEPARATOR), $filePathParts);
    }

    /**
     * Load rule-set config from a .php file.
     */
    private function getConfigFromPhpFile(string $fileName): RuleSet
    {
        $config = include $fileName;
        $ruleSet = $this->initRuleSet($fileName, $config['name'] ?? null);
        $this->configRuleSetWith($ruleSet, $config);

        return $ruleSet;
    }

    /**
     * Load rule-set config from a .yaml file.
     */
    private function getConfigFromYamlFile(string $fileName): RuleSet
    {
        $config = Yaml::parseFile($fileName);
        $ruleSet = $this->initRuleSet($fileName, $config['name'] ?? null);
        $this->configRuleSetWith($ruleSet, $config);

        return $ruleSet;
    }

    /**
     * Load rule-set config from a .xml file.
     */
    private function getConfigFromXmlFile(string $fileName): RuleSet
    {
        // Hide error messages
        $libxml = libxml_use_internal_errors(true);

        $xml = simplexml_load_string(file_get_contents($fileName));
        if ($xml === false) {
            // Reset error handling to previous setting
            libxml_use_internal_errors($libxml);

            throw new RuntimeException(trim(libxml_get_last_error()->message));
        }

        $ruleSet = $this->initRuleSet($fileName, $xml['name'] ?? null);

        foreach ($xml->children() as $node) {
            if ($node->getName() === 'php-includepath') {
                $this->addIncludePath($fileName, (string)$node);
            }
        }

        foreach ($xml->children() as $node) {
            if ($node->getName() === 'description') {
                $ruleSet->setDescription((string)$node);
            } elseif ($node->getName() === 'rule') {
                $this->parseRuleNode($ruleSet, $node);
            }
        }

        return $ruleSet;
    }

    /**
     * Configure RuleSet according to given array config.
     */
    private function configRuleSetWith(RuleSet $ruleSet, array $config): void
    {
        $ruleSet->setDescription((string)($config['description'] ?? ''));

        foreach (((array)$config['php-includepath'] ?? []) as $value) {
            $this->addIncludePath($ruleSet->getFileName(), (string)$value);
        }

        foreach (((array)$config['rules'] ?? []) as $rule) {
            $this->parseRuleFromConfig($ruleSet, $rule);
        }
    }

    /**
     * Create a RuleSet with initial properties: filename, name (inferred from filename if null)
     * and propagate the RuleSetFactory strict state into this new RuleSet.
     */
    private function initRuleSet(string $fileName, mixed $name): RuleSet
    {
        $ruleSet = new RuleSet();
        $ruleSet->setFileName($fileName);
        $ruleSet->setName((string)($name ?? pathinfo($fileName, PATHINFO_FILENAME)));

        if ($this->strict) {
            $ruleSet->setStrict();
        }

        return $ruleSet;
    }

    /**
     * Add given path to PHP include paths.
     */
    private function addIncludePath(string $fileName, string $includePath): void
    {
        $directory = dirname($fileName) . DIRECTORY_SEPARATOR . $includePath;

        if (is_dir($directory)) {
            $includePath = realpath($directory);
        }

        set_include_path(get_include_path() . PATH_SEPARATOR . $includePath);
    }

    /**
     * This method parses a single rule that was included from a different
     * rule-set.
     */
    private function parseRuleFromConfig(RuleSet $ruleSet, array $config): void
    {
        $ref = (string)$config['ref'];

        $fileName = substr($ref, 0, strpos($ref, '.xml/') + 4);
        $fileName = $this->createRuleSetFileName($fileName);

        $ruleName = substr($ref, strpos($ref, '.xml/') + 5);

        $ruleSetFactory = new RuleSetFactory();

        $ruleSetRef = $ruleSetFactory->createSingleRuleSet($fileName);
        $rule = $ruleSetRef->getRuleByName($ruleName);
        $name = trim((string)($config['name'] ?? ''));

        if ($name !== '') {
            $rule->setName($name);
        }

        $message = trim((string)($config['message'] ?? ''));

        if ($message !== '') {
            $rule->setMessage($message);
        }

        $externalInfoUrl = trim((string)($config['externalInfoUrl'] ?? ''));

        if ($externalInfoUrl !== '') {
            $rule->setExternalInfoUrl($externalInfoUrl);
        }

        $this->parseRuleProperties($rule, $config);

        if ($rule->getPriority() <= $this->minimumPriority && $rule->getPriority() >= $this->maximumPriority) {
            $ruleSet->addRule($rule);
        }
    }
}
